module Scene.Emit (emit) where

import Context.App
import Context.Env qualified as Env
import Context.Gensym qualified as Gensym
import Context.StaticText (StaticTextInfo)
import Context.Throw qualified as Throw
import Control.Monad
import Data.ByteString.Builder
import Data.ByteString.Builder qualified as L
import Data.ByteString.Lazy qualified as L
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Entity.Builder
import Entity.DeclarationName qualified as DN
import Entity.DefiniteDescription qualified as DD
import Entity.Ident
import Entity.LowComp qualified as LC
import Entity.LowComp.EmitOp qualified as EOP
import Entity.LowComp.EmitValue
import Entity.LowType qualified as LT
import Entity.LowType.EmitLowType
import Scene.LowComp.Reduce qualified as LowComp

emit :: (DN.DeclEnv, [LC.Def], Maybe LC.Comp, [StaticTextInfo]) -> App L.ByteString
emit (declEnv, defList, mMainTerm, staticTextList) = do
  baseSize <- Env.getBaseSize'
  let staticTextList' = map (emitStaticText baseSize) staticTextList
  case mMainTerm of
    Just mainTerm -> do
      let declStrList = emitDeclarations declEnv
      mainStrList <- emitMain mainTerm
      defStrList <- concat <$> mapM emitDefinitions defList
      return $ L.toLazyByteString $ unlinesL $ declStrList <> staticTextList' <> mainStrList <> defStrList
    Nothing -> do
      let declStrList = emitDeclarations declEnv
      defStrList <- concat <$> mapM emitDefinitions defList
      return $ L.toLazyByteString $ unlinesL $ declStrList <> staticTextList' <> defStrList

emitStaticText :: Int -> StaticTextInfo -> Builder
emitStaticText baseSize (from, (text, len)) = do
  "@"
    <> DD.toBuilder from
    <> " = private unnamed_addr constant "
    <> emitLowType (LT.textType baseSize len)
    <> " {"
    <> "i"
    <> intDec baseSize
    <> " 0, "
    <> "i"
    <> intDec baseSize
    <> " "
    <> intDec len
    <> ", "
    <> emitLowType (LT.textTypeInner len)
    <> " c\""
    <> TE.encodeUtf8Builder text
    <> "\""
    <> "}"

emitDeclarations :: DN.DeclEnv -> [Builder]
emitDeclarations declEnv = do
  map declToBuilder $ List.sort $ HashMap.toList declEnv

emitDefinitions :: LC.Def -> App [Builder]
emitDefinitions (name, (args, body)) = do
  let args' = map (emitValue . LC.VarLocal) args
  body' <- LowComp.reduce IntMap.empty body
  emitDefinition "ptr" (DD.toBuilder name) args' body'

emitMain :: LC.Comp -> App [Builder]
emitMain mainTerm = do
  mainTerm' <- LowComp.reduce IntMap.empty mainTerm
  mainType <- Env.getMainType
  emitDefinition (TE.encodeUtf8Builder mainType) "main" [] mainTerm'

declToBuilder :: (DN.DeclarationName, ([LT.LowType], LT.LowType)) -> Builder
declToBuilder (name, (dom, cod)) = do
  let name' = DN.toBuilder name
  "declare fastcc "
    <> emitLowType cod
    <> " @"
    <> name'
    <> "("
    <> unwordsC (map emitLowType dom)
    <> ")"

emitDefinition :: Builder -> Builder -> [Builder] -> LC.Comp -> App [Builder]
emitDefinition retType name args asm = do
  let header = sig retType name args <> " {"
  content <- emitLowComp retType asm
  let footer = "}"
  return $ [header] <> content <> [footer]

sig :: Builder -> Builder -> [Builder] -> Builder
sig retType name args =
  "define fastcc " <> retType <> " @" <> name <> showLocals args

emitBlock :: Builder -> Ident -> LC.Comp -> App [Builder]
emitBlock funName (I (_, i)) asm = do
  a <- emitLowComp funName asm
  return $ emitLabel ("_" <> intDec i) : a

emitLowComp :: Builder -> LC.Comp -> App [Builder]
emitLowComp retType lowComp =
  case lowComp of
    LC.Return d ->
      return $ emitOp $ unwordsL ["ret", retType, emitValue d]
    LC.TailCall f args -> do
      tmp <- Gensym.newIdentFromText "tmp"
      let op =
            emitOp $
              unwordsL
                [ emitValue (LC.VarLocal tmp),
                  "=",
                  "tail call fastcc ptr",
                  emitValue f <> showArgs args
                ]
      ret <- emitLowComp retType $ LC.Return (LC.VarLocal tmp)
      return $ op <> ret
    LC.Switch (d, lowType) defaultBranch branchList -> do
      defaultLabel <- Gensym.newIdentFromText "default"
      labelList <- constructLabelList branchList
      let op =
            emitOp $
              unwordsL
                [ "switch",
                  emitLowType lowType,
                  emitValue d <> ",",
                  "label",
                  emitValue (LC.VarLocal defaultLabel),
                  showBranchList lowType $ zip (map fst branchList) labelList
                ]
      let asmList = map snd branchList
      xs <-
        forM ((defaultLabel, defaultBranch) : zip labelList asmList) $
          uncurry (emitBlock retType)
      return $ op <> concat xs
    LC.Cont op cont -> do
      targetPlatform <- Env.getTargetPlatform
      case EOP.emitLowOp targetPlatform op of
        Left err ->
          Throw.raiseCritical' $ T.pack err
        Right s -> do
          let str = emitOp s
          a <- emitLowComp retType cont
          return $ str <> a
    LC.Let x op cont -> do
      targetPlatform <- Env.getTargetPlatform
      case EOP.emitLowOp targetPlatform op of
        Left err ->
          Throw.raiseCritical' $ T.pack err
        Right s -> do
          let str = emitOp $ emitValue (LC.VarLocal x) <> " = " <> s
          a <- emitLowComp retType cont
          return $ str <> a
    LC.Unreachable ->
      return $ emitOp $ unwordsL ["unreachable"]

emitOp :: Builder -> [Builder]
emitOp s =
  ["  " <> s]

emitLabel :: Builder -> Builder
emitLabel s =
  s <> ":"

constructLabelList :: [a] -> App [Ident]
constructLabelList input =
  case input of
    [] ->
      return []
    (_ : rest) -> do
      label <- Gensym.newIdentFromText "case"
      labelList <- constructLabelList rest
      return $ label : labelList

showBranchList :: LT.LowType -> [(Integer, Ident)] -> Builder
showBranchList lowType xs =
  "[" <> unwordsL (map (uncurry (showBranch lowType)) xs) <> "]"

showBranch :: LT.LowType -> Integer -> Ident -> Builder
showBranch lowType i label =
  emitLowType lowType
    <> " "
    <> integerDec i
    <> ", label "
    <> emitValue (LC.VarLocal label)
