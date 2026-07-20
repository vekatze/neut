module Kernel.Emit.Emit
  ( Handle,
    new,
    emit,
  )
where

import App.App (App)
import App.Run (raiseError')
import Console.Handle qualified as Console
import Console.ReportMode qualified as Report
import Control.Monad (when)
import Data.ByteString.Builder
import Data.ByteString.Builder qualified as L
import Data.ByteString.Lazy qualified as L
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Gensym.CreateHandle qualified as Gensym
import Gensym.Handle qualified as GensymHandle
import Kernel.Common.Allocator (Allocator, allocatorSpec)
import Kernel.Common.Const
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.ModulePath qualified as ModulePath
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Target (Target)
import Kernel.Common.Trace qualified as Trace
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
import Logger.Debug qualified as Logger

data Handle = Handle
  { gensymHandle :: GensymHandle.Handle,
    globalHandle :: Global.Handle,
    allocator :: Allocator,
    traceConfig :: Trace.Config
  }

new :: GensymHandle.Handle -> Global.Handle -> Target -> App Handle
new gensymHandle globalHandle target = do
  allocator <- Env.getAllocatorByTarget (Global.envHandle globalHandle) target
  let traceReport = Console.getTraceConfig $ Global.consoleHandle globalHandle
  traceConfig <- either raiseError' return $ Trace.new (Env.getMainModule $ Global.envHandle globalHandle) traceReport
  return $ Handle {..}

emit :: Handle -> LC.LowCode -> IO L.ByteString
emit h lowCode = do
  case lowCode of
    LC.LowCodeMain mainDef lowCodeInfo -> do
      main <- emitMain h mainDef
      let moduleHeader = emitModuleHeader h
      let argDef = emitArgDef
      (header, body) <- emitLowCodeInfo h lowCodeInfo
      return $ buildByteString $ moduleHeader ++ header ++ argDef ++ main ++ body
    LC.LowCodeNormal lowCodeInfo -> do
      let moduleHeader = emitModuleHeader h
      let argDecl = emitArgDecl
      (header, body) <- emitLowCodeInfo h lowCodeInfo
      return $ buildByteString $ moduleHeader ++ header ++ argDecl ++ body

emitModuleHeader :: Handle -> [Builder]
emitModuleHeader h = do
  let platformHandle = Global.platformHandle $ globalHandle h
  let targetTriple = Platform.getClangTargetTriple platformHandle
  ["target triple = \"" <> TE.encodeUtf8Builder (T.pack targetTriple) <> "\""]

emitLowCodeInfo :: Handle -> LC.LowCodeInfo -> IO ([Builder], [Builder])
emitLowCodeInfo h (declEnv, defList, staticTextList) = do
  let declStrList = emitDeclarations declEnv
  let baseSize = Platform.getDataSize (Global.platformHandle (globalHandle h))
  let staticTextList' = concatMap (emitStaticText baseSize) staticTextList
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

emitStaticText :: DS.DataSize -> StaticTextInfo -> [Builder]
emitStaticText baseSize (from, (text, len)) = do
  let headerName = TE.encodeUtf8Builder ("\"" <> from <> "\"")
  let payloadName = TE.encodeUtf8Builder ("\"" <> from <> ".payload\"")
  let wordType = "i" <> intDec (DS.reify baseSize)
  let payloadType = emitLowType (LT.textTypeInner len)
  let payload =
        "@"
          <> payloadName
          <> " = private unnamed_addr constant "
          <> payloadType
          <> " c\""
          <> text
          <> "\""
  let header =
        "@"
          <> headerName
          <> " = private unnamed_addr constant "
          <> emitLowType (LT.textType baseSize)
          <> " {"
          <> wordType
          <> " 0, "
          <> wordType
          <> " "
          <> intDec len
          <> ", ptr "
          <> "@"
          <> payloadName
          <> "}"
  [payload, header]

emitDeclarations :: DN.DeclEnv -> [Builder]
emitDeclarations declEnv = do
  map declToBuilder $ List.sort $ HashMap.toList declEnv

emitDefinitions :: Handle -> LC.Def -> IO [Builder]
emitDefinitions h (name, LC.DefContent {codType = codType, args = args, body = body}) = do
  definitionGensymHandle <- Gensym.createHandle
  args' <- mapM (Gensym.newIdentFromIdent definitionGensymHandle) args
  let sub = IntMap.fromList $ zipWith (\from to -> (toInt from, LC.VarLocal to)) args args'
  let reduceHandle = Reduce.new definitionGensymHandle
  body' <- Reduce.reduce reduceHandle sub body
  let args'' = showFuncArgs $ map (emitValue . LC.VarLocal) args'
  emitDefinition h definitionGensymHandle (Just name) (emitLowType codType) (DD.toBuilder name) args'' body'

emitMain :: Handle -> LC.DefContent -> IO [Builder]
emitMain h (LC.DefContent {codType = codType, args = args, body = body}) = do
  let args' = showFuncArgs $ map (emitValue . LC.VarLocal) args
  emitDefinition h (gensymHandle h) Nothing (emitLowType codType) "main" args' body

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

emitDefinition :: Handle -> GensymHandle.Handle -> Maybe DD.DefiniteDescription -> Builder -> Builder -> Builder -> LC.Comp -> IO [Builder]
emitDefinition h gensymHandle maybeName retType name args asm = do
  let header = sig retType name args <> " {"
  emitLowCompHandle <- EmitLowComp.new gensymHandle (globalHandle h) retType (allocatorSpec $ allocator h)
  content <- EmitLowComp.emitLowComp emitLowCompHandle asm
  let footer = "}"
  let definition = [header] <> content <> [footer]
  case maybeName of
    Just traceName -> do
      modulePathMap <- ModulePath.get $ Global.modulePathHandle (globalHandle h)
      when (Trace.matches (traceConfig h) modulePathMap Report.LLVMPhase traceName) $ do
        Logger.trace (Global.loggerHandle (globalHandle h)) $
          "[llvm]\n" <> TE.decodeUtf8 (L.toStrict $ L.toLazyByteString $ unlinesL definition)
    Nothing ->
      return ()
  return definition

sig :: Builder -> Builder -> Builder -> Builder
sig retType name args =
  "define fastcc " <> retType <> " @" <> name <> args
