module Scene.Emit (emit) where

import Context.App
import Context.Env qualified as Env
import Context.Gensym qualified as Gensym
import Context.StaticText (StaticTextInfo)
import Context.Throw qualified as Throw
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Builder
import Data.ByteString.Builder qualified as L
import Data.ByteString.Lazy qualified as L
import Data.HashMap.Strict qualified as HashMap
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Entity.Builder
import Entity.Const
import Entity.DeclarationName qualified as DN
import Entity.DefiniteDescription qualified as DD
import Entity.Ident
import Entity.Ident.Reify
import Entity.LowComp qualified as LC
import Entity.LowComp.EmitOp qualified as EOP
import Entity.LowComp.EmitValue
import Entity.LowType qualified as LT
import Entity.LowType.EmitLowType
import Scene.LowComp.Reduce qualified as LowComp

emit :: (DN.DeclEnv, [LC.Def], Maybe LC.DefContent, [StaticTextInfo]) -> App L.ByteString
emit (declEnv, defList, mMainTerm, staticTextList) = do
  baseSize <- Env.getBaseSize'
  let staticTextList' = map (emitStaticText baseSize) staticTextList
  case mMainTerm of
    Just mainDef -> do
      let argc = emitGlobal unsafeArgcName LT.Pointer LC.Null
      let argv = emitGlobal unsafeArgvName LT.Pointer LC.Null
      let declStrList = emitDeclarations declEnv
      mainStrList <- emitMain mainDef
      defStrList <- concat <$> mapM emitDefinitions defList
      return $ L.toLazyByteString $ unlinesL $ declStrList <> staticTextList' <> [argc, argv] <> mainStrList <> defStrList
    Nothing -> do
      let argc = emitGlobalExt unsafeArgcName LT.Pointer
      let argv = emitGlobalExt unsafeArgvName LT.Pointer
      let declStrList = emitDeclarations declEnv
      defStrList <- concat <$> mapM emitDefinitions defList
      return $ L.toLazyByteString $ unlinesL $ declStrList <> staticTextList' <> [argc, argv] <> defStrList

emitGlobal :: T.Text -> LT.LowType -> LC.Value -> Builder
emitGlobal name lt v =
  "@"
    <> TE.encodeUtf8Builder name
    <> " = global "
    <> emitLowType lt
    <> " "
    <> emitValue v

emitGlobalExt :: T.Text -> LT.LowType -> Builder
emitGlobalExt name lt =
  "@"
    <> TE.encodeUtf8Builder name
    <> " = external global "
    <> emitLowType lt

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

emitMain :: LC.DefContent -> App [Builder]
emitMain (args, body) = do
  mainType <- Env.getMainType
  let args' = map (emitValue . LC.VarLocal) args
  emitDefinition (TE.encodeUtf8Builder mainType) "main" args' body

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
  ctx <- newCtx retType
  content <- emitLowComp ctx asm
  let footer = "}"
  return $ [header] <> content <> [footer]

sig :: Builder -> Builder -> [Builder] -> Builder
sig retType name args =
  "define fastcc " <> retType <> " @" <> name <> showLocals args

type Label =
  Ident

data EmitCtx = EmitCtx
  { phiInfo :: Maybe (Ident, Label),
    currentLabel :: Maybe Label,
    retType :: Builder,
    labelMap :: IORef (IntMap.IntMap Ident)
  }

newCtx :: Builder -> App EmitCtx
newCtx retTypeBuilder = do
  emptyMapRef <- liftIO $ newIORef IntMap.empty
  return
    EmitCtx
      { phiInfo = Nothing,
        currentLabel = Nothing,
        retType = retTypeBuilder,
        labelMap = emptyMapRef
      }

emitLowComp :: EmitCtx -> LC.Comp -> App [Builder]
emitLowComp ctx lowComp =
  case lowComp of
    LC.Return d ->
      case phiInfo ctx of
        Nothing ->
          return $ emitOp $ unwordsL ["ret", retType ctx, emitValue d]
        Just (phiSrcVar, rendezvous) -> do
          lowOp <- emitLowOp (emitValue (LC.VarLocal phiSrcVar) <> " = ") $ LC.Bitcast d LT.Pointer LT.Pointer
          let brOp = emitOp $ unwordsL ["br", "label", emitValue (LC.VarLocal rendezvous)]
          return $ lowOp <> brOp
    LC.TailCall f args -> do
      tmp <- Gensym.newIdentFromText "tmp"
      let op =
            emitOp $
              unwordsL [emitValue (LC.VarLocal tmp), "=", "tail call fastcc ptr", emitValue f <> showArgs args]
      ret <- emitLowComp ctx $ LC.Return (LC.VarLocal tmp)
      return $ op <> ret
    LC.Switch (d, lowType) defaultBranch branchList (phiTgt, cont) -> do
      defaultLabel <- Gensym.newIdentFromText "default"
      labelList <- constructLabelList branchList
      let switchOpStr =
            emitOp $
              unwordsL
                [ "switch",
                  emitLowType lowType,
                  emitValue d <> ",",
                  "label",
                  emitValue (LC.VarLocal defaultLabel),
                  showBranchList lowType $ zip (map fst branchList) labelList
                ]
      let labelBranchList = zip labelList (map snd branchList) <> [(defaultLabel, defaultBranch)]
      confluenceLabel <- Gensym.newIdentFromText "confluence"
      case currentLabel ctx of
        Nothing ->
          return ()
        Just current -> do
          -- bypass switch clauses and get the confluence block
          liftIO $ modifyIORef' (labelMap ctx) $ IntMap.insert (toInt current) confluenceLabel
      phiSrcVarList <- mapM (const $ Gensym.newIdentFromText "phi") labelBranchList
      blockAsmList <-
        forM (zip labelBranchList phiSrcVarList) $ \((label, branch), phiSrcVar) -> do
          let newPhiInfo = Just (phiSrcVar, confluenceLabel)
          a <- emitLowComp (ctx {phiInfo = newPhiInfo, currentLabel = Just label}) branch
          return $ emitLabel ("_" <> intDec (toInt label)) : a
      let allLabelList = map fst labelBranchList
      currentLabelMap <- liftIO $ readIORef $ labelMap ctx
      let resolvedLabelList = resolveLabelList currentLabelMap allLabelList
      let phiOp =
            unwordsL
              [ "phi",
                emitLowType LT.Pointer,
                emitPhiList $ zip phiSrcVarList resolvedLabelList
              ]
      let phiOpStr = emitOp $ emitValue (LC.VarLocal phiTgt) <> " = " <> phiOp
      rendezvousBlock <- do
        a <- emitLowComp (ctx {currentLabel = Just confluenceLabel}) cont
        return $ emitLabel ("_" <> intDec (toInt confluenceLabel)) : phiOpStr <> a
      return $ switchOpStr <> concat blockAsmList <> rendezvousBlock
    LC.Cont op cont -> do
      lowOp <- emitLowOp "" op
      a <- emitLowComp ctx cont
      return $ lowOp <> a
    LC.Let x op cont -> do
      lowOp <- emitLowOp (emitValue (LC.VarLocal x) <> " = ") op
      a <- emitLowComp ctx cont
      return $ lowOp <> a
    LC.Unreachable -> do
      emitLowComp ctx $ LC.Return LC.Null

resolveLabelList :: IntMap.IntMap Ident -> [Ident] -> [Ident]
resolveLabelList labelMap xs =
  case xs of
    [] ->
      []
    x : rest ->
      case IntMap.lookup (toInt x) labelMap of
        Just y ->
          resolveLabelList labelMap $ y : rest
        Nothing ->
          x : resolveLabelList labelMap rest

emitLowOp :: Builder -> LC.Op -> App [Builder]
emitLowOp prefix op = do
  targetPlatform <- Env.getTargetPlatform
  case EOP.emitLowOp targetPlatform op of
    Left err ->
      Throw.raiseCritical' $ T.pack err
    Right s -> do
      return $ emitOp $ prefix <> s

emitPhiList :: [(Ident, Ident)] -> Builder
emitPhiList identLabelList =
  case identLabelList of
    [] ->
      ""
    [(ident, label)] ->
      "[" <> emitValue (LC.VarLocal ident) <> ", " <> emitValue (LC.VarLocal label) <> "]"
    (ident, label) : rest ->
      showIdentLabel ident label <> ", " <> emitPhiList rest

showIdentLabel :: Ident -> Ident -> Builder
showIdentLabel ident label =
  "[" <> emitValue (LC.VarLocal ident) <> ", " <> emitValue (LC.VarLocal label) <> "]"

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
