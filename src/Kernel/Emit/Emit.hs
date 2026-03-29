module Kernel.Emit.Emit
  ( Handle,
    new,
    emit,
  )
where

import App.App (App)
import Data.ByteString.Builder
import Data.ByteString.Builder qualified as L
import Data.ByteString.Lazy qualified as L
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Gensym.CreateHandle qualified as Gensym
import Kernel.Common.Allocator (Allocator, allocatorSpec)
import Kernel.Common.Const
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Target (Target)
import Kernel.Emit.Builder
import Kernel.Emit.Internal.LowComp qualified as EmitLowComp
import Kernel.Emit.LowType
import Kernel.Emit.LowValue
import Language.Common.BaseLowType qualified as BLT
import Language.Common.CreateSymbol qualified as Gensym
import Language.Common.DataSize qualified as DS
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.Ident.Reify
import Language.Common.LowType qualified as LT
import Language.Common.LowType.FromBaseLowType qualified as LT
import Language.LowComp.DeclarationName qualified as DN
import Language.LowComp.LowComp qualified as LC
import Language.LowComp.Reduce qualified as Reduce

data Handle = Handle
  { globalHandle :: Global.Handle,
    allocator :: Allocator
  }

new :: Global.Handle -> Target -> App Handle
new globalHandle target = do
  allocator <- Env.getAllocatorByTarget (Global.envHandle globalHandle) target
  return $ Handle {..}

emit :: Handle -> LC.LowCode -> IO L.ByteString
emit h lowCode = do
  case lowCode of
    LC.LowCodeMain mainDef lowCodeInfo -> do
      main <- emitMain h mainDef
      let argDef = emitArgDef
      (header, body) <- emitLowCodeInfo h lowCodeInfo
      return $ buildByteString $ header ++ argDef ++ main ++ body
    LC.LowCodeNormal lowCodeInfo -> do
      let argDecl = emitArgDecl
      (header, body) <- emitLowCodeInfo h lowCodeInfo
      return $ buildByteString $ header ++ argDecl ++ body

emitLowCodeInfo :: Handle -> LC.LowCodeInfo -> IO ([Builder], [Builder])
emitLowCodeInfo h (declEnv, defList, staticTextList) = do
  let declStrList = emitDeclarations declEnv
  let baseSize = Platform.getDataSize (Global.platformHandle (globalHandle h))
  let staticTextList' = map (emitStaticText baseSize) staticTextList
  defStrList <- concat <$> mapM (emitDefinitions h) defList
  return (declStrList <> staticTextList', defStrList)

emitArgDecl :: [Builder]
emitArgDecl = do
  let argc = emitGlobalExt unsafeArgcName LT.Pointer
  let argv = emitGlobalExt unsafeArgvName LT.Pointer
  [argc, argv]

emitArgDef :: [Builder]
emitArgDef = do
  let argc = emitGlobal unsafeArgcName LT.Pointer LC.Null
  let argv = emitGlobal unsafeArgvName LT.Pointer LC.Null
  [argc, argv]

buildByteString :: [Builder] -> L.ByteString
buildByteString =
  L.toLazyByteString . unlinesL

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

type StaticTextInfo = (T.Text, (Builder, Int))

emitStaticText :: DS.DataSize -> StaticTextInfo -> Builder
emitStaticText baseSize (from, (text, len)) = do
  "@"
    <> TE.encodeUtf8Builder ("\"" <> from <> "\"")
    <> " = private unnamed_addr constant "
    <> emitLowType (LT.textType baseSize len)
    <> " {"
    <> "i"
    <> intDec (DS.reify baseSize)
    <> " 0, "
    <> "i"
    <> intDec (DS.reify baseSize)
    <> " "
    <> intDec len
    <> ", "
    <> emitLowType (LT.textTypeInner len)
    <> " c\""
    <> text
    <> "\""
    <> "}"

emitDeclarations :: DN.DeclEnv -> [Builder]
emitDeclarations declEnv = do
  map declToBuilder $ List.sort $ HashMap.toList declEnv

emitDefinitions :: Handle -> LC.Def -> IO [Builder]
emitDefinitions h (name, LC.DefContent {codType = codType, args = args, body = body}) = do
  gensymHandle <- Gensym.createHandle
  let h' = h {globalHandle = (globalHandle h) {Global.gensymHandle = gensymHandle}}
  args' <- mapM (Gensym.newIdentFromIdent (Global.gensymHandle (globalHandle h'))) args
  let sub = IntMap.fromList $ zipWith (\from to -> (toInt from, LC.VarLocal to)) args args'
  let reduceHandle = Reduce.new (Global.gensymHandle (globalHandle h'))
  body' <- Reduce.reduce reduceHandle sub body
  let args'' =
        case codType of
          LT.Void ->
            showFuncArgsWithSRet $ map (emitValue . LC.VarLocal) args'
          _ ->
            showFuncArgs $ map (emitValue . LC.VarLocal) args'
  emitDefinition h' (emitLowType codType) (DD.toBuilder name) args'' body'

emitMain :: Handle -> LC.DefContent -> IO [Builder]
emitMain h (LC.DefContent {codType = codType, args = args, body = body}) = do
  let args' = showFuncArgs $ map (emitValue . LC.VarLocal) args
  emitDefinition h (emitLowType codType) "main" args' body

declToBuilder :: (DN.DeclarationName, ([BLT.BaseLowType], FCT.ForeignCodType BLT.BaseLowType)) -> Builder
declToBuilder (name, (dom, cod)) = do
  let name' = DN.toBuilder name
  "declare "
    <> emitLowType (FCT.fromForeignCodType cod)
    <> " @"
    <> name'
    <> "("
    <> unwordsC (map (emitLowType . LT.fromBaseLowType) dom)
    <> ")"

emitDefinition :: Handle -> Builder -> Builder -> Builder -> LC.Comp -> IO [Builder]
emitDefinition h retType name args asm = do
  let header = sig retType name args <> " {"
  emitLowCompHandle <- EmitLowComp.new (globalHandle h) retType (allocatorSpec $ allocator h)
  content <- EmitLowComp.emitLowComp emitLowCompHandle asm
  let footer = "}"
  return $ [header] <> content <> [footer]

sig :: Builder -> Builder -> Builder -> Builder
sig retType name args =
  "define fastcc " <> retType <> " @" <> name <> args
