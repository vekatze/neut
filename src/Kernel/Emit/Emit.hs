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
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Gensym.CreateHandle qualified as Gensym
import Gensym.Handle qualified as GensymHandle
import Kernel.Common.Allocator (Allocator, AllocatorKind (..), allocatorFamily, allocatorForeignList, allocatorSpec)
import Kernel.Common.Arch qualified as Arch
import Kernel.Common.Const
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.ModulePath qualified as ModulePath
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Platform qualified as P
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
import Language.Common.ExternalName qualified as EN
import Language.Common.Foreign qualified as F
import Language.Common.ForeignCodType qualified as FCT
import Language.Common.Ident.Reify
import Language.Common.LowType qualified as LT
import Language.Common.LowType.FromBaseLowType qualified as LT
import Language.Common.PrimNumSize.ToInt (floatSizeToInt, intSizeToInt)
import Language.Common.PrimType qualified as PT
import Language.Common.SlotSize
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
      let argDef = emitArgDef (getDataSize h)
      (header, body) <- emitLowCodeInfo h lowCodeInfo
      let attributeGroups = emitAttributeGroups (getArch h)
      return $ buildByteString $ moduleHeader ++ header ++ argDef ++ main ++ body ++ attributeGroups
    LC.LowCodeNormal lowCodeInfo -> do
      let moduleHeader = emitModuleHeader h
      let argDecl = emitArgDecl
      (header, body) <- emitLowCodeInfo h lowCodeInfo
      let attributeGroups = emitAttributeGroups (getArch h)
      return $ buildByteString $ moduleHeader ++ header ++ argDecl ++ body ++ attributeGroups

emitModuleHeader :: Handle -> [Builder]
emitModuleHeader h = do
  let platformHandle = Global.platformHandle $ globalHandle h
  let targetTriple = Platform.getClangTargetTriple platformHandle
  ["target triple = \"" <> TE.encodeUtf8Builder (T.pack targetTriple) <> "\""]

emitLowCodeInfo :: Handle -> LC.LowCodeInfo -> IO ([Builder], [Builder])
emitLowCodeInfo h (declEnv, defList, staticTextList, staticDataList, exportList) = do
  let declStrList = emitDeclarations h declEnv
  let staticTextList' = concatMap emitStaticText staticTextList
  let staticDataList' = map (emitStaticData (getDataSize h)) staticDataList
  defStrList <- concat <$> mapM (emitDefinitions h) defList
  let exportStrList = concatMap (emitExport h) exportList ++ emitExportRoots exportList
  return (declStrList <> staticTextList' <> staticDataList', defStrList <> exportStrList)

emitExport :: Handle -> LC.ExportInfo -> [Builder]
emitExport h (EN.ExternalName extName, dd, domList, cod) = do
  let name' = TE.encodeUtf8Builder extName
  let argList = map (\i -> "%a" <> intDec i) [0 .. length domList - 1]
  let params = unwordsC (zipWith (\t arg -> emitLowType t <> " " <> arg) domList argList)
  let trailingArgs = replicate LC.internalTrailingArgCount (emitLowType LT.slotLowType <> " 0")
  let callArgs = unwordsC (zipWith (\t arg -> attachAttributes (emitLowType t) (internalArgAttributes t) <> " " <> arg) domList argList ++ trailingArgs)
  let cod' = emitLowType cod
  let exportAttributes =
        case getArch h of
          Arch.Wasm32 ->
            ["\"wasm-export-name\"=\"" <> name' <> "\""]
          _ ->
            []
  let attrs = mconcat $ map (" " <>) $ exportAttributes ++ archFunctionAttributes (getArch h)
  [ "define " <> cod' <> " @\"" <> name' <> "\"(" <> params <> ")" <> attrs <> " {",
    "  %ret = tail call fastcc " <> cod' <> " @" <> DD.toBuilder dd <> "(" <> callArgs <> ")",
    "  ret " <> cod' <> " %ret",
    "}"
    ]

emitExportRoots :: [LC.ExportInfo] -> [Builder]
emitExportRoots exportList =
  case exportList of
    [] ->
      []
    _ -> do
      let names = map (\(EN.ExternalName extName, _, _, _) -> "ptr @\"" <> TE.encodeUtf8Builder extName <> "\"") exportList
      let arrayType = "[" <> intDec (length exportList) <> " x ptr]"
      ["@llvm.used = appending global " <> arrayType <> " [" <> unwordsC names <> "], section \"llvm.metadata\""]

argcGlobalType :: LT.LowType
argcGlobalType =
  LT.slotLowType

emitArgDecl :: [Builder]
emitArgDecl = do
  let argc = emitGlobalExt unsafeArgcName argcGlobalType
  let argv = emitGlobalExt unsafeArgvName LT.Pointer
  [argc, argv]

emitArgDef :: DS.DataSize -> [Builder]
emitArgDef baseSize = do
  let argc = emitGlobal baseSize unsafeArgcName argcGlobalType (LC.Int 0)
  let argv = emitGlobal baseSize unsafeArgvName LT.Pointer LC.Null
  [argc, argv]

buildByteString :: [Builder] -> L.ByteString
buildByteString =
  L.toLazyByteString . unlinesL

emitGlobal :: DS.DataSize -> T.Text -> LT.LowType -> LC.Value -> Builder
emitGlobal baseSize name lt v =
  "@"
    <> TE.encodeUtf8Builder name
    <> " = global "
    <> emitLowType lt
    <> " "
    <> emitValue baseSize v

emitGlobalExt :: T.Text -> LT.LowType -> Builder
emitGlobalExt name lt =
  "@"
    <> TE.encodeUtf8Builder name
    <> " = external global "
    <> emitLowType lt

type StaticTextInfo = (T.Text, (Builder, Int))

emitStaticData :: DS.DataSize -> LC.StaticDataInfo -> Builder
emitStaticData baseSize (name, slots) = do
  let fields = concatMap (emitStaticSlot baseSize) slots
  "@"
    <> TE.encodeUtf8Builder ("\"" <> name <> "\"")
    <> " = private unnamed_addr constant <{"
    <> unwordsC (map fst fields)
    <> "}> <{"
    <> unwordsC (map snd fields)
    <> "}>, align "
    <> intDec slotByteSize

slotTypeBuilder :: Builder
slotTypeBuilder =
  emitLowType LT.slotLowType

emitStaticSlot :: DS.DataSize -> LC.StaticData -> [(Builder, Builder)]
emitStaticSlot baseSize slot =
  case slot of
    LC.StaticNull ->
      [asSlot "0"]
    LC.StaticSymbol name ->
      emitAddressSlot baseSize $ "@" <> TE.encodeUtf8Builder ("\"" <> name <> "\"")
    LC.StaticGlobal dd ->
      emitAddressSlot baseSize $ "@" <> DD.toBuilder dd
    LC.StaticInt size value ->
      [asSlot $ integerDec $ widenToSlot (intSizeToInt size) value]
    LC.StaticFloat size value ->
      [asSlot $ integerDec $ widenToSlot (floatSizeToInt size) (floatToBits size value)]

asSlot :: Builder -> (Builder, Builder)
asSlot value =
  (slotTypeBuilder, slotTypeBuilder <> " " <> value)

emitAddressSlot :: DS.DataSize -> Builder -> [(Builder, Builder)]
emitAddressSlot baseSize address = do
  let paddingBitSize = slotBitSize - DS.reify baseSize
  let pointerField = ("ptr", "ptr " <> address)
  if paddingBitSize == 0
    then [pointerField]
    else do
      let paddingType = "i" <> intDec paddingBitSize
      [pointerField, (paddingType, paddingType <> " 0")]

widenToSlot :: Int -> Integer -> Integer
widenToSlot bitSize value =
  if bitSize == slotBitSize
    then value
    else value `mod` (2 ^ bitSize)

emitStaticText :: StaticTextInfo -> [Builder]
emitStaticText (from, (text, len)) = do
  let headerName = TE.encodeUtf8Builder ("\"" <> from <> "\"")
  let payloadName = TE.encodeUtf8Builder ("\"" <> from <> ".payload\"")
  let wordType = emitLowType (LT.PrimNum (PT.Int slotIntSize))
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
          <> emitLowType LT.textType
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

emitDeclarations :: Handle -> DN.DeclEnv -> [Builder]
emitDeclarations h declEnv = do
  map (declToBuilder h) $ List.sort $ HashMap.toList declEnv

emitDefinitions :: Handle -> LC.Def -> IO [Builder]
emitDefinitions h (name, LC.DefContent {codType = codType, args = args, body = body}) = do
  definitionGensymHandle <- Gensym.createHandle
  args' <- mapM (Gensym.newIdentFromIdent definitionGensymHandle . fst) args
  let sub = IntMap.fromList $ zipWith (\from to -> (toInt from, LC.VarLocal to)) (map fst args) args'
  let reduceHandle = Reduce.new definitionGensymHandle
  body' <- Reduce.reduce reduceHandle sub body
  let args'' = showInternalArgs (getDataSize h) $ zipWith (\(_, t) x -> (t, LC.VarLocal x)) args args'
  emitDefinition h definitionGensymHandle True (Just name) codType (DD.toBuilder name) args'' body'

emitMain :: Handle -> LC.DefContent -> IO [Builder]
emitMain h (LC.DefContent {codType = codType, args = args, body = body}) = do
  let renderArg (x, t) = emitLowType t <> " " <> emitValue (getDataSize h) (LC.VarLocal x)
  let args' = "(" <> unwordsC (map renderArg args) <> ")"
  emitDefinition h (gensymHandle h) False Nothing codType (mainSymbol (getArch h)) args' body

mainSymbol :: Arch.Arch -> Builder
mainSymbol arch =
  case arch of
    Arch.Wasm32 ->
      "__main_argc_argv"
    _ ->
      "main"

getArch :: Handle -> Arch.Arch
getArch h =
  P.arch $ Platform.getPlatform $ Global.platformHandle $ globalHandle h

getDataSize :: Handle -> DS.DataSize
getDataSize h =
  Platform.getDataSize $ Global.platformHandle $ globalHandle h

declToBuilder :: Handle -> (DN.DeclarationName, ([BLT.BaseLowType], FCT.ForeignCodType BLT.BaseLowType, DN.Variadicity)) -> Builder
declToBuilder h (name, (dom, cod, variadicity)) = do
  let codType = FCT.fromForeignCodType cod
  let isInternal =
        case name of
          DN.In {} ->
            True
          DN.Ext {} ->
            False
  let maybeKind = allocatorKindOf h name dom cod
  let callConvAttributes = if isInternal then ["fastcc"] else []
  let returnType = if isInternal then emitInternalReturnType codType else emitLowType codType
  let returnAttributes = maybe [] allocatorReturnAttributes maybeKind
  let signature =
        returnType
          <> " @"
          <> DN.toBuilder name
          <> emitDeclarationArgs isInternal maybeKind variadicity dom
  attachAttributes "declare" $
    callConvAttributes
      ++ returnAttributes
      ++ [signature]
      ++ maybe [] (allocatorFunctionAttributes (allocator h)) maybeKind
      ++ wasmImportAttributes (getArch h) name

wasmImportAttributes :: Arch.Arch -> DN.DeclarationName -> [Builder]
wasmImportAttributes arch name =
  case (arch, name) of
    (Arch.Wasm32, DN.Ext (EN.ExternalName extName))
      | not ("llvm." `T.isPrefixOf` extName) -> do
          let name' = TE.encodeUtf8Builder extName
          ["\"wasm-import-module\"=\"env\" \"wasm-import-name\"=\"" <> name' <> "\""]
    _ ->
      []

emitDeclarationArgs :: Bool -> Maybe AllocatorKind -> DN.Variadicity -> [BLT.BaseLowType] -> Builder
emitDeclarationArgs isInternal maybeKind variadicity dom = do
  let renderArg index t = do
        let attributes =
              if isInternal
                then internalArgAttributes t
                else maybe [] (`allocatorArgAttributes` index) maybeKind
        attachAttributes (emitLowType t) attributes
  let renderedArgs = zipWith renderArg [0 ..] (map LT.fromBaseLowType dom)
  let renderedArgs' =
        case variadicity of
          DN.Variadic ->
            renderedArgs ++ ["..."]
          DN.Fixed ->
            renderedArgs
  "(" <> unwordsC renderedArgs' <> ")"

allocatorKindOf ::
  Handle ->
  DN.DeclarationName ->
  [BLT.BaseLowType] ->
  FCT.ForeignCodType BLT.BaseLowType ->
  Maybe AllocatorKind
allocatorKindOf h name dom cod = do
  let dataSize = Platform.getDataSize $ Global.platformHandle $ globalHandle h
  let foreignList = allocatorForeignList dataSize (allocatorSpec $ allocator h)
  let matches (kind, F.Foreign _ allocatorName expectedDom expectedCod) =
        if DN.Ext allocatorName == name && dom == expectedDom && cod == expectedCod
          then Just kind
          else Nothing
  listToMaybe $ mapMaybe matches foreignList

allocatorReturnAttributes :: AllocatorKind -> [Builder]
allocatorReturnAttributes kind =
  case kind of
    Free ->
      []
    _ ->
      ["noalias", "noundef"]

allocatorArgAttributes :: AllocatorKind -> Int -> [Builder]
allocatorArgAttributes kind index = do
  let isAllocPtr =
        case kind of
          Realloc ->
            index == 0
          Free ->
            index == 0
          _ ->
            False
  if isAllocPtr
    then ["allocptr", "noundef"]
    else ["noundef"]

allocatorFunctionAttributes :: Allocator -> AllocatorKind -> [Builder]
allocatorFunctionAttributes allocator kind = do
  let effects =
        case kind of
          Malloc ->
            ["allocsize(0)", "allockind(\"alloc,uninitialized\")", "memory(inaccessiblemem: readwrite)"]
          Calloc ->
            ["allocsize(0,1)", "allockind(\"alloc,zeroed\")", "memory(inaccessiblemem: readwrite)"]
          Realloc ->
            ["allocsize(1)", "allockind(\"realloc\")", "memory(argmem: readwrite, inaccessiblemem: readwrite)"]
          Free ->
            ["allockind(\"free\")", "memory(argmem: readwrite, inaccessiblemem: readwrite)"]
  let family = TE.encodeUtf8Builder $ allocatorFamily allocator
  ["nounwind", "willreturn"] ++ effects ++ ["\"alloc-family\"=\"" <> family <> "\""]

emitDefinition :: Handle -> GensymHandle.Handle -> Bool -> Maybe DD.DefiniteDescription -> LT.LowType -> Builder -> Builder -> LC.Comp -> IO [Builder]
emitDefinition h gensymHandle isInternal maybeName retType name args asm = do
  let attributes = archFunctionAttributes (getArch h) ++ tailCallAttributes maybeName asm
  let header = sig isInternal retType name args attributes <> " {"
  emitLowCompHandle <- EmitLowComp.new gensymHandle (globalHandle h) (emitLowType retType) (allocatorSpec $ allocator h)
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

sig :: Bool -> LT.LowType -> Builder -> Builder -> [Builder] -> Builder
sig isInternal retType name args functionAttributes = do
  let callConvAttributes = if isInternal then ["fastcc"] else []
  let returnType = if isInternal then emitInternalReturnType retType else emitLowType retType
  attachAttributes "define" $ callConvAttributes ++ [returnType <> " @" <> name <> args] ++ functionAttributes

tailCallAttributes :: Maybe DD.DefiniteDescription -> LC.Comp -> [Builder]
tailCallAttributes maybeName body =
  case maybeName of
    Just self
      | hasMustTailCall body,
        hasNonTailSelfCall self body ->
          ["\"disable-tail-calls\"=\"true\""]
    _ ->
      []

hasMustTailCall :: LC.Comp -> Bool
hasMustTailCall =
  anyComp $ \comp ->
    case comp of
      LC.TailCall mustTail _ _ _ ->
        mustTail
      _ ->
        False

hasNonTailSelfCall :: DD.DefiniteDescription -> LC.Comp -> Bool
hasNonTailSelfCall self =
  anyComp $ \comp ->
    case comp of
      LC.Let _ (LC.Call _ _ (LC.VarGlobal callee) _) _ ->
        callee == self
      LC.Cont (LC.Call _ _ (LC.VarGlobal callee) _) _ ->
        callee == self
      _ ->
        False

anyComp :: (LC.Comp -> Bool) -> LC.Comp -> Bool
anyComp p comp =
  p comp || any (anyComp p) (subComps comp)

subComps :: LC.Comp -> [LC.Comp]
subComps comp =
  case comp of
    LC.Let _ _ cont ->
      [cont]
    LC.Cont _ cont ->
      [cont]
    LC.Switch _ _ defaultBranch branchList _ cont ->
      defaultBranch : map snd branchList ++ [cont]
    _ ->
      []

archAttributeGroupName :: Builder
archAttributeGroupName =
  "#0"

archAttributeList :: Arch.Arch -> [Builder]
archAttributeList arch =
  case arch of
    Arch.Wasm32 ->
      ["\"target-features\"=\"+tail-call\""]
    _ ->
      []

archFunctionAttributes :: Arch.Arch -> [Builder]
archFunctionAttributes arch =
  case archAttributeList arch of
    [] ->
      []
    _ ->
      [archAttributeGroupName]

emitAttributeGroups :: Arch.Arch -> [Builder]
emitAttributeGroups arch =
  case archAttributeList arch of
    [] ->
      []
    attributeList ->
      ["attributes " <> archAttributeGroupName <> " = { " <> unwordsC attributeList <> " }"]
