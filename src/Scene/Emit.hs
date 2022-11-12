module Scene.Emit
  ( emit,
    Context,
  )
where

import qualified Context.Env as Env
import qualified Context.Gensym as Gensym
import qualified Context.Throw as Throw
import Control.Monad
import Data.ByteString.Builder
import qualified Data.ByteString.Builder as L
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Entity.DeclarationName as DN
import qualified Entity.DefiniteDescription as DD
import qualified Entity.ExternalName as EN
import Entity.Ident
import qualified Entity.LowComp as LC
import qualified Entity.LowComp.Reduce as LowComp
import qualified Entity.LowType as LT
import Entity.PrimNumSize
import Entity.PrimNumSize.ToInt
import Entity.PrimOp
import Entity.PrimOp.OpSet
import qualified Entity.PrimType as PT
import qualified Entity.TargetPlatform as TP
import Numeric.Half
import qualified System.Info as System

class
  ( Gensym.Context m,
    Throw.Context m,
    Env.Context m,
    LowComp.Context m
  ) =>
  Context m

emit :: Context m => (DN.DeclEnv, [LC.Def], Maybe LC.Comp) -> m L.ByteString
emit (declEnv, defList, mMainTerm) = do
  case mMainTerm of
    Just mainTerm -> do
      let declStrList = emitDeclarations declEnv
      mainStrList <- emitMain mainTerm
      defStrList <- concat <$> mapM emitDefinitions defList
      return $ L.toLazyByteString $ unlinesL $ declStrList <> mainStrList <> defStrList
    Nothing -> do
      let declStrList = emitDeclarations declEnv
      defStrList <- concat <$> mapM emitDefinitions defList
      return $ L.toLazyByteString $ unlinesL $ declStrList <> defStrList

emitDeclarations :: DN.DeclEnv -> [Builder]
emitDeclarations declEnv = do
  map declToBuilder $ List.sort $ HashMap.toList declEnv

emitDefinitions :: Context m => LC.Def -> m [Builder]
emitDefinitions (name, (args, body)) = do
  let args' = map (showLowValue . LC.VarLocal) args
  (is, body') <- LowComp.reduce IntMap.empty Map.empty body
  Env.setNopFreeSet is
  emitDefinition "i8*" (DD.toBuilder name) args' body'

emitMain :: Context m => LC.Comp -> m [Builder]
emitMain mainTerm = do
  (is, mainTerm') <- LowComp.reduce IntMap.empty Map.empty mainTerm
  Env.setNopFreeSet is
  emitDefinition "i64" "main" [] mainTerm'

declToBuilder :: (DN.DeclarationName, ([LT.LowType], LT.LowType)) -> Builder
declToBuilder (name, (dom, cod)) = do
  let name' = DN.toBuilder name
  "declare fastcc "
    <> showLowType cod
    <> " @"
    <> name'
    <> "("
    <> showItems showLowType dom
    <> ")"

emitDefinition :: Context m => Builder -> Builder -> [Builder] -> LC.Comp -> m [Builder]
emitDefinition retType name args asm = do
  let header = sig retType name args <> " {"
  content <- emitLowComp retType asm
  let footer = "}"
  return $ [header] <> content <> [footer]

sig :: Builder -> Builder -> [Builder] -> Builder
sig retType name args =
  "define fastcc " <> retType <> " @" <> name <> showLocals args

emitBlock :: Context m => Builder -> Ident -> LC.Comp -> m [Builder]
emitBlock funName (I (_, i)) asm = do
  a <- emitLowComp funName asm
  return $ emitLabel ("_" <> intDec i) : a

emitLowComp :: Context m => Builder -> LC.Comp -> m [Builder]
emitLowComp retType lowComp =
  case lowComp of
    LC.Return d ->
      emitRet retType d
    LC.TailCall f args -> do
      tmp <- Gensym.newIdentFromText "tmp"
      op <-
        emitOp $
          unwordsL
            [ showLowValue (LC.VarLocal tmp),
              "=",
              "tail call fastcc i8*",
              showLowValue f <> showArgs args
            ]
      a <- emitRet retType (LC.VarLocal tmp)
      return $ op <> a
    LC.Switch (d, lowType) defaultBranch branchList -> do
      defaultLabel <- Gensym.newIdentFromText "default"
      labelList <- constructLabelList branchList
      op <-
        emitOp $
          unwordsL
            [ "switch",
              showLowType lowType,
              showLowValue d <> ",",
              "label",
              showLowValue (LC.VarLocal defaultLabel),
              showBranchList lowType $ zip (map fst branchList) labelList
            ]
      let asmList = map snd branchList
      xs <-
        forM (zip labelList asmList <> [(defaultLabel, defaultBranch)]) $
          uncurry (emitBlock retType)
      return $ op <> concat xs
    LC.Cont op cont -> do
      s <- emitLowOp op
      str <- emitOp s
      a <- emitLowComp retType cont
      return $ str <> a
    LC.Let x op cont -> do
      s <- emitLowOp op
      str <- emitOp $ showLowValue (LC.VarLocal x) <> " = " <> s
      a <- emitLowComp retType cont
      return $ str <> a
    LC.Unreachable ->
      emitOp $ unwordsL ["unreachable"]

emitLowOp :: Context m => LC.Op -> m Builder
emitLowOp lowOp =
  case lowOp of
    LC.Call d ds ->
      return $ unwordsL ["call fastcc i8*", showLowValue d <> showArgs ds]
    LC.GetElementPtr (basePtr, n) is ->
      return $
        unwordsL
          [ "getelementptr",
            showLowTypeAsIfNonPtr n <> ",",
            showLowType n,
            showLowValue basePtr <> ",",
            showIndex is
          ]
    LC.Bitcast d fromType toType ->
      emitConvOp "bitcast" d fromType toType
    LC.IntToPointer d fromType toType ->
      emitConvOp "inttoptr" d fromType toType
    LC.PointerToInt d fromType toType ->
      emitConvOp "ptrtoint" d fromType toType
    LC.Load d lowType ->
      return $
        unwordsL
          [ "load",
            showLowType lowType <> ",",
            showLowTypeAsIfPtr lowType,
            showLowValue d
          ]
    LC.Store t d1 d2 ->
      return $
        unwordsL
          [ "store",
            showLowType t,
            showLowValue d1 <> ",",
            showLowTypeAsIfPtr t,
            showLowValue d2
          ]
    LC.Alloc d _ ->
      return $ unwordsL ["call fastcc", "i8*", "@malloc(i8* " <> showLowValue d <> ")"]
    LC.Free d _ j -> do
      nopFreeSet <- Env.getNopFreeSet
      if S.member j nopFreeSet
        then return "bitcast i8* null to i8*" -- nop
        else return $ unwordsL ["call fastcc", "i8*", "@free(i8* " <> showLowValue d <> ")"]
    LC.Syscall num ds ->
      emitSyscallOp num ds
    LC.PrimOp (PrimOp op domList cod) args -> do
      let op' = TE.encodeUtf8Builder op
      case (S.member op unaryOpSet, S.member op convOpSet, S.member op binaryOpSet, S.member op cmpOpSet) of
        (True, _, _, _) ->
          emitUnaryOp (head domList) op' (head args)
        (_, True, _, _) ->
          emitConvOp op' (head args) (LT.PrimNum $ head domList) (LT.PrimNum cod)
        (_, _, True, _) ->
          emitBinaryOp (head domList) op' (head args) (args !! 1)
        (_, _, _, True) ->
          emitBinaryOp (head domList) op' (head args) (args !! 1)
        _ ->
          Throw.raiseCritical' $ "unknown primitive: " <> op

emitUnaryOp :: Monad m => PT.PrimType -> Builder -> LC.Value -> m Builder
emitUnaryOp t inst d =
  return $ unwordsL [inst, showPrimNumForEmit t, showLowValue d]

emitBinaryOp :: Monad m => PT.PrimType -> Builder -> LC.Value -> LC.Value -> m Builder
emitBinaryOp t inst d1 d2 =
  return $
    unwordsL [inst, showPrimNumForEmit t, showLowValue d1 <> ",", showLowValue d2]

emitConvOp :: Monad m => Builder -> LC.Value -> LT.LowType -> LT.LowType -> m Builder
emitConvOp cast d dom cod =
  return $
    unwordsL [cast, showLowType dom, showLowValue d, "to", showLowType cod]

emitSyscallOp :: Context m => Integer -> [LC.Value] -> m Builder
emitSyscallOp num ds = do
  regList <- getRegList
  case System.arch of
    "x86_64" -> do
      let args = (LC.Int num, LT.PrimNum $ PT.Int (IntSize 64)) : zip ds (repeat LT.voidPtr)
      let argStr = "(" <> showIndex args <> ")"
      let regStr = "\"=r" <> showRegList (take (length args) regList) <> "\""
      return $
        unwordsL ["call fastcc i8* asm sideeffect \"syscall\",", regStr, argStr]
    "aarch64" -> do
      let args = (LC.Int num, LT.PrimNum $ PT.Int (IntSize 64)) : zip ds (repeat LT.voidPtr)
      let argStr = "(" <> showIndex args <> ")"
      let regStr = "\"=r" <> showRegList (take (length args) regList) <> "\""
      return $
        unwordsL ["call fastcc i8* asm sideeffect \"svc 0\",", regStr, argStr]
    targetArch ->
      Throw.raiseCritical' $ "unsupported target arch: " <> T.pack (show targetArch)

emitOp :: Monad m => Builder -> m [Builder]
emitOp s =
  return ["  " <> s]

emitRet :: Monad m => Builder -> LC.Value -> m [Builder]
emitRet retType d =
  emitOp $ unwordsL ["ret", retType, showLowValue d]

emitLabel :: Builder -> Builder
emitLabel s =
  s <> ":"

constructLabelList :: Gensym.Context m => [a] -> m [Ident]
constructLabelList input =
  case input of
    [] ->
      return []
    (_ : rest) -> do
      label <- Gensym.newIdentFromText "case"
      labelList <- constructLabelList rest
      return $ label : labelList

showRegList :: [Builder] -> Builder
showRegList regList =
  case regList of
    [] ->
      ""
    (s : ss) ->
      ",{" <> s <> "}" <> showRegList ss

showBranchList :: LT.LowType -> [(Integer, Ident)] -> Builder
showBranchList lowType xs =
  "[" <> unwordsL (map (uncurry (showBranch lowType)) xs) <> "]"

showIndex :: [(LC.Value, LT.LowType)] -> Builder
showIndex idxList =
  case idxList of
    [] ->
      ""
    [(d, t)] ->
      showLowType t <> " " <> showLowValue d
    ((d, t) : dts) ->
      showIndex [(d, t)] <> ", " <> showIndex dts

showBranch :: LT.LowType -> Integer -> Ident -> Builder
showBranch lowType i label =
  showLowType lowType
    <> " "
    <> integerDec i
    <> ", label "
    <> showLowValue (LC.VarLocal label)

showArg :: LC.Value -> Builder
showArg d =
  "i8* " <> showLowValue d

showLocal :: Builder -> Builder
showLocal x =
  "i8* " <> x

showArgs :: [LC.Value] -> Builder
showArgs ds =
  "(" <> showItems showArg ds <> ")"

showLocals :: [Builder] -> Builder
showLocals ds =
  "(" <> showItems showLocal ds <> ")"

showLowTypeAsIfPtr :: LT.LowType -> Builder
showLowTypeAsIfPtr t =
  showLowType t <> "*"

showLowTypeAsIfNonPtr :: LT.LowType -> Builder
showLowTypeAsIfNonPtr lowType =
  case lowType of
    LT.PrimNum primNum ->
      showPrimNumForEmit primNum
    LT.Struct ts ->
      "{" <> showItems showLowType ts <> "}"
    LT.Function ts t ->
      showLowType t <> " (" <> showItems showLowType ts <> ")"
    LT.Array i t -> do
      let s = showLowType t
      "[" <> intDec i <> " x " <> s <> "]"
    LT.Pointer t ->
      showLowType t

getRegList :: Context m => m [Builder]
getRegList = do
  targetPlatform <- Env.getTargetPlatform
  let platform = TP.platform targetPlatform
  case platform of
    "x86_64-linux" ->
      return ["rax", "rdi", "rsi", "rdx", "rcx", "r8", "r9"]
    "arm64-linux" ->
      return ["x8", "x0", "x1", "x2", "x3", "x4", "x5"]
    "x86_64-darwin" ->
      return ["rax", "rdi", "rsi", "rdx", "r10", "r8", "r9"]
    _ ->
      Throw.raiseError' $ "unsupported target: " <> T.pack platform

showLowType :: LT.LowType -> Builder
showLowType lowType =
  case lowType of
    LT.PrimNum primNum ->
      showPrimNumForEmit primNum
    LT.Struct ts ->
      "{" <> showItems showLowType ts <> "}"
    LT.Function ts t ->
      showLowType t <> " (" <> showItems showLowType ts <> ")"
    LT.Array i t -> do
      let s = showLowType t
      "[" <> intDec i <> " x " <> s <> "]"
    LT.Pointer t ->
      showLowType t <> "*"

showPrimNumForEmit :: PT.PrimType -> Builder
showPrimNumForEmit lowType =
  case lowType of
    PT.Int i ->
      "i" <> intDec (intSizeToInt i)
    PT.Float FloatSize16 ->
      "half"
    PT.Float FloatSize32 ->
      "float"
    PT.Float FloatSize64 ->
      "double"

showLowValue :: LC.Value -> Builder
showLowValue lowValue =
  case lowValue of
    LC.VarLocal (I (_, i)) ->
      "%_" <> intDec i
    LC.VarGlobal globalName ->
      "@" <> DD.toBuilder globalName
    LC.VarExternal extName ->
      "@" <> EN.toBuilder extName
    LC.Int i ->
      integerDec i
    LC.Float FloatSize16 x -> do
      let x' = realToFrac x :: Half
      "0x" <> doubleHexFixed (realToFrac x')
    LC.Float FloatSize32 x -> do
      let x' = realToFrac x :: Float
      "0x" <> doubleHexFixed (realToFrac x')
    LC.Float FloatSize64 x -> do
      let x' = realToFrac x :: Double
      "0x" <> doubleHexFixed (realToFrac x')
    LC.Null ->
      "null"

showItems :: (a -> Builder) -> [a] -> Builder
showItems f itemList =
  case itemList of
    [] ->
      ""
    [a] ->
      f a
    a : as ->
      f a <> ", " <> showItems f as

{-# INLINE unwordsL #-}
unwordsL :: [Builder] -> Builder
unwordsL strList =
  case strList of
    [] ->
      ""
    [b] ->
      b
    b : bs ->
      b <> " " <> unwordsL bs

{-# INLINE unlinesL #-}
unlinesL :: [Builder] -> Builder
unlinesL strList =
  case strList of
    [] ->
      ""
    [b] ->
      b
    b : bs ->
      b <> "\n" <> unlinesL bs
