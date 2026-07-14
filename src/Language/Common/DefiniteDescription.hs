{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Common.DefiniteDescription
  ( DefiniteDescription,
    reify,
    new,
    localLocator,
    globalLocator,
    bodySegments,
    baseNameText,
    extendBody,
    appendText,
    strictGlobalLocator,
    globalParts,
    newByGlobalLocator,
    getFormDD,
    getNodeDD,
    getLeafDD,
    getRootDD,
    getClosureEnvDD,
    getLambdaDD,
    getMuDD,
    getKnotDD,
    getLocalMetaDD,
    imm,
    baseTypes,
    cls,
    toBuilder,
    llvmGlobalLocator,
    unconsDD,
    makeResourceName,
  )
where

import Data.Binary
import Data.ByteString.Builder
import Data.Hashable
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Language.Common.BaseName qualified as BN
import Language.Common.Const
import Language.Common.Ident
import Language.Common.LocalLocator qualified as LL
import Language.Common.ModuleDigest qualified as MD
import Language.Common.ModuleID qualified as MID
import Language.Common.SourceLocator qualified as SL
import Language.Common.StrictGlobalLocator qualified as SGL

-- the textual form of a definite description is:
--
--     module.path::source.path::body.path
--
-- where `::` separates the three chunks and `.` separates segments in a chunk.
newtype DefiniteDescription
  = MakeDefiniteDescription {reify :: T.Text}
  deriving (Show, Eq, Ord, Binary, Hashable)

make :: SGL.StrictGlobalLocator -> LL.LocalLocator -> T.Text -> DefiniteDescription
make gl body suffix =
  MakeDefiniteDescription $ SGL.reify gl <> doubleColon <> LL.reify body <> suffix

reflectModuleID :: T.Text -> MID.ModuleID
reflectModuleID modulePathText = do
  case modulePathText of
    "this" ->
      MID.Main
    "base" ->
      MID.Base
    _ ->
      MID.Library $ MD.ModuleDigest modulePathText

globalParts :: DefiniteDescription -> Maybe (T.Text, T.Text, T.Text)
globalParts (MakeDefiniteDescription text) = do
  let (modulePathText, moduleRest) = T.breakOn doubleColon text
  if T.null moduleRest
    then Nothing
    else do
      let sourceAndBody = T.drop (T.length doubleColon) moduleRest
      let (sourceText, sourceRest) = T.breakOn doubleColon sourceAndBody
      if T.null sourceRest
        then Nothing
        else do
          let bodyText = T.drop (T.length doubleColon) sourceRest
          return (modulePathText, sourceText, bodyText)

splitBodySuffix :: T.Text -> (T.Text, T.Text)
splitBodySuffix =
  T.breakOn "#"

new :: SGL.StrictGlobalLocator -> LL.LocalLocator -> DefiniteDescription
new gl ll =
  make gl ll ""

newByGlobalLocator :: SGL.StrictGlobalLocator -> BN.BaseName -> DefiniteDescription
newByGlobalLocator gl name = do
  new gl $ LL.new name

{-# INLINE toLowName #-}
toLowName :: DefiniteDescription -> T.Text
toLowName dd =
  wrapWithQuote $ reify dd

{-# INLINE wrapWithQuote #-}
wrapWithQuote :: T.Text -> T.Text
wrapWithQuote x =
  "\"" <> x <> "\""

-- main.foo.bar
-- ~> main.foo.bar#form
appendLocalName :: DefiniteDescription -> BN.BaseName -> DefiniteDescription
appendLocalName dd name = do
  appendText dd $ "#" <> BN.reify name

appendText :: DefiniteDescription -> T.Text -> DefiniteDescription
appendText (MakeDefiniteDescription text) suffix =
  MakeDefiniteDescription $ text <> suffix

getFormDD :: DefiniteDescription -> DefiniteDescription
getFormDD dd =
  appendLocalName dd BN.form

getNodeDD :: DefiniteDescription -> DefiniteDescription
getNodeDD dd =
  appendLocalName dd BN.node

getLeafDD :: DefiniteDescription -> DefiniteDescription
getLeafDD dd =
  appendLocalName dd BN.leaf

getRootDD :: DefiniteDescription -> DefiniteDescription
getRootDD dd =
  appendLocalName dd BN.root

getClosureEnvDD :: DefiniteDescription -> DefiniteDescription
getClosureEnvDD closureName =
  appendText closureName "#env"

getLambdaDD :: DefiniteDescription -> Maybe T.Text -> Int -> DefiniteDescription
getLambdaDD dd mName i =
  appendLocalName dd (BN.lambdaName mName i)

getMuDD :: DefiniteDescription -> Ident -> Int -> DefiniteDescription
getMuDD dd x i =
  appendLocalName dd (BN.muName x i)

getKnotDD :: DefiniteDescription -> Int -> DefiniteDescription
getKnotDD dd i =
  appendText dd $ "#knot." <> T.pack (show i)

getLocalMetaDD :: Int -> DefiniteDescription
getLocalMetaDD i =
  MakeDefiniteDescription $ "#meta." <> T.pack (show i)

unconsDD :: DefiniteDescription -> (MID.ModuleID, T.Text)
unconsDD dd = do
  case globalParts dd of
    Just (modulePathText, sourceText, bodyText) ->
      (reflectModuleID modulePathText, sourceText <> doubleColon <> bodyText)
    Nothing ->
      error $ "Language.Common.DefiniteDescription.unconsDD: local name: " <> T.unpack (reify dd)

-- module.path::source.path (the textual form of the strict global locator)
globalLocator :: DefiniteDescription -> T.Text
globalLocator dd = do
  case globalParts dd of
    Just (modulePathText, sourceText, _) ->
      modulePathText <> doubleColon <> sourceText
    Nothing ->
      error $ "Language.Common.DefiniteDescription.globalLocator: local name: " <> T.unpack (reify dd)

strictGlobalLocator :: DefiniteDescription -> SGL.StrictGlobalLocator
strictGlobalLocator dd = do
  case globalParts dd of
    Just (modulePathText, sourceText, _) -> do
      let sourceSegments = map BN.fromText $ T.splitOn nsSep sourceText
      case SL.fromBaseNameList sourceSegments of
        Just sourceLocator ->
          SGL.new (reflectModuleID modulePathText) sourceLocator
        Nothing ->
          error $ "Language.Common.DefiniteDescription.strictGlobalLocator: invalid source path: " <> T.unpack sourceText
    Nothing ->
      error $ "Language.Common.DefiniteDescription.strictGlobalLocator: local name: " <> T.unpack (reify dd)

-- the in-file body path of the name (e.g. `ns.func` in `module::src::ns.func`)
localLocator :: DefiniteDescription -> T.Text
localLocator dd = do
  case globalParts dd of
    Just (_, _, bodyText) ->
      bodyText
    Nothing ->
      reify dd

bodySegments :: DefiniteDescription -> [BN.BaseName]
bodySegments dd = do
  case globalParts dd of
    Just (_, _, bodyWithSuffix) -> do
      let (bodyText, _) = splitBodySuffix bodyWithSuffix
      map BN.fromText $ T.splitOn nsSep bodyText
    Nothing ->
      map BN.fromText $ T.splitOn nsSep $ reify dd

baseNameText :: DefiniteDescription -> T.Text
baseNameText dd = do
  case globalParts dd of
    Just (_, _, bodyWithSuffix) -> do
      let (bodyText, suffix) = splitBodySuffix bodyWithSuffix
      case reverse $ T.splitOn nsSep bodyText of
        baseName : _ ->
          baseName <> suffix
        [] ->
          error "Language.Common.DefiniteDescription.baseNameText: empty body path"
    Nothing ->
      reify dd

extendBody :: DefiniteDescription -> [BN.BaseName] -> DefiniteDescription
extendBody dd rest = do
  case (globalParts dd, rest) of
    (_, []) ->
      dd
    (Just (modulePathText, sourceText, bodyWithSuffix), _) -> do
      let (bodyText, suffix) = splitBodySuffix bodyWithSuffix
      let extension = T.intercalate nsSep $ map BN.reify rest
      MakeDefiniteDescription $ modulePathText <> doubleColon <> sourceText <> doubleColon <> bodyText <> nsSep <> extension <> suffix
    (Nothing, _) ->
      dd

makeResourceName :: DefiniteDescription -> Int -> DefiniteDescription
makeResourceName baseDD resourceID = do
  let resourceDD = newByGlobalLocator (strictGlobalLocator baseDD) BN.resource
  appendText resourceDD $ "#" <> T.pack (show resourceID)

imm :: DefiniteDescription
imm =
  newByGlobalLocator (SGL.baseGlobalLocatorOf SL.internalLocator) BN.imm

baseTypes :: [DefiniteDescription]
baseTypes =
  [imm, cls]

cls :: DefiniteDescription
cls =
  newByGlobalLocator (SGL.baseGlobalLocatorOf SL.internalLocator) BN.cls

toBuilder :: DefiniteDescription -> Builder
toBuilder dd =
  TE.encodeUtf8Builder $ toLowName dd

llvmGlobalLocator :: T.Text
llvmGlobalLocator =
  "base::llvm"
