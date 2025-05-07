module Kernel.Common.Rule.Module
  ( Module (..),
    MainModule (..),
    SomePath,
    LocatorName,
    AliasPresetMap,
    Foreign (..),
    Dependency (..),
    TargetName,
    PresetSummary,
    keyAntecedent,
    keyArchive,
    keyBuildOption,
    keyCache,
    keyCompileOption,
    keyDependency,
    keyDigest,
    keyEnablePreset,
    keyExtraContent,
    keyForeign,
    keyForeignInput,
    keyForeignOutput,
    keyForeignScript,
    keyInlineLimit,
    keyLinkOption,
    keyMain,
    keyMirror,
    keyPrefix,
    keyPreset,
    keySource,
    keyStatic,
    keyTarget,
    keyZen,
    getSourceDir,
    getTargetPathList,
    getTargetPath,
    getArchiveDir,
    getExtraContents,
    insertDependency,
    getAliasListWithEnabledPresets,
    toDefaultEns,
    ppDirPath,
    getDigestMap,
    getDigestFromModulePath,
    reifyPresetMap,
    getRelPathFromSourceDir,
    _m,
    getModuleRootDir,
    getTarget,
    stylize,
    extractModule,
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.Catch
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict qualified as Map
import Data.List (sort)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes, maybeToList)
import Data.Text qualified as T
import Ens.Rule.Ens qualified as E
import Error.Rule.Error
import Kernel.Common.Rule.ClangOption qualified as CL
import Kernel.Common.Rule.Const
import Kernel.Common.Rule.ModuleURL
import Kernel.Common.Rule.Target qualified as Target
import Kernel.Common.Rule.ZenConfig
import Language.Common.Rule.BaseName qualified as BN
import Language.Common.Rule.GlobalLocator qualified as GL
import Language.Common.Rule.ModuleAlias qualified as MA
import Language.Common.Rule.ModuleDigest
import Language.Common.Rule.ModuleID qualified as MID
import Language.Common.Rule.SourceLocator qualified as SL
import Logger.Rule.Hint (Hint, internalHint)
import Path
import SyntaxTree.Rule.Series qualified as SE
import System.FilePath qualified as FP

type SomePath a =
  Either (Path a Dir) (Path a File)

type LocatorName =
  T.Text

type PresetMap =
  Map.HashMap LocatorName [BN.BaseName]

type AliasPresetMap =
  Map.HashMap T.Text PresetMap

data Dependency = Dependency
  { dependencyMirrorList :: [ModuleURL],
    dependencyDigest :: ModuleDigest,
    dependencyPresetEnabled :: Bool
  }
  deriving (Show)

data Foreign = Foreign
  { input :: [SomePath Rel],
    output :: [Path Rel File],
    script :: [T.Text]
  }
  deriving (Show)

type TargetName =
  T.Text

data Module = Module
  { moduleID :: MID.ModuleID,
    moduleSourceDir :: Path Rel Dir,
    moduleTarget :: Map.HashMap TargetName Target.TargetSummary,
    moduleZenConfig :: ZenConfig,
    moduleArchiveDir :: Path Rel Dir,
    moduleCacheDir :: Path Rel Dir,
    moduleDependency :: Map.HashMap MA.ModuleAlias Dependency,
    moduleExtraContents :: [SomePath Rel],
    moduleAntecedents :: [ModuleDigest],
    moduleLocation :: Path Abs File,
    moduleForeign :: Foreign,
    moduleStaticFiles :: Map.HashMap T.Text (Path Rel File),
    modulePrefixMap :: Map.HashMap BN.BaseName (MA.ModuleAlias, SL.SourceLocator),
    moduleInlineLimit :: Maybe Int,
    modulePresetMap :: PresetMap
  }
  deriving (Show)

newtype MainModule
  = MainModule Module
  deriving (Show)

extractModule :: MainModule -> Module
extractModule (MainModule m) =
  m

keyArchive :: T.Text
keyArchive =
  "archive"

keyCache :: T.Text
keyCache =
  "cache"

keySource :: T.Text
keySource =
  "source"

keyTarget :: T.Text
keyTarget =
  "target"

keyZen :: T.Text
keyZen =
  "zen"

keyMain :: T.Text
keyMain =
  "main"

keyBuildOption :: T.Text
keyBuildOption =
  "build-option"

keyCompileOption :: T.Text
keyCompileOption =
  "compile-option"

keyLinkOption :: T.Text
keyLinkOption =
  "link-option"

keyDependency :: T.Text
keyDependency =
  "dependency"

keyDigest :: T.Text
keyDigest =
  "digest"

keyMirror :: T.Text
keyMirror =
  "mirror"

keyEnablePreset :: T.Text
keyEnablePreset =
  "enable-preset"

keyExtraContent :: T.Text
keyExtraContent =
  "extra-content"

keyAntecedent :: T.Text
keyAntecedent =
  "antecedent"

keyStatic :: T.Text
keyStatic =
  "static"

keyForeign :: T.Text
keyForeign =
  "foreign"

keyForeignInput :: T.Text
keyForeignInput =
  "input"

keyForeignOutput :: T.Text
keyForeignOutput =
  "output"

keyForeignScript :: T.Text
keyForeignScript =
  "script"

keyPrefix :: T.Text
keyPrefix =
  "prefix"

keyInlineLimit :: T.Text
keyInlineLimit =
  "inline-limit"

keyPreset :: T.Text
keyPreset =
  "preset"

getSourceDir :: Module -> Path Abs Dir
getSourceDir baseModule =
  getModuleRootDir baseModule </> moduleSourceDir baseModule

getTargetPathList :: Module -> [Path Abs File]
getTargetPathList baseModule = do
  let moduleSourceDir = getSourceDir baseModule
  let sourceLocatorList = Map.elems $ moduleTarget baseModule
  map ((moduleSourceDir </>) . SL.reify . Target.entryPoint) sourceLocatorList

getTargetPath :: Module -> T.Text -> Maybe (Path Abs File)
getTargetPath baseModule target = do
  let moduleSourceDir = getSourceDir baseModule
  sourceLocator <- Map.lookup target (moduleTarget baseModule)
  return $ moduleSourceDir </> SL.reify (Target.entryPoint sourceLocator)

getArchiveDir :: Module -> Path Abs Dir
getArchiveDir baseModule =
  getModuleRootDir baseModule </> moduleArchiveDir baseModule

getExtraContents :: Module -> [SomePath Rel]
getExtraContents baseModule = do
  moduleExtraContents baseModule

getModuleRootDir :: Module -> Path Abs Dir
getModuleRootDir baseModule =
  parent $ moduleLocation baseModule

insertDependency :: ([ModuleURL], [ModuleDigest]) -> ([ModuleURL], [ModuleDigest]) -> ([ModuleURL], [ModuleDigest])
insertDependency (mirrorList1, digest1) (mirrorList2, _) = do
  (nubOrd $ mirrorList1 ++ mirrorList2, digest1)

getDigestMap :: Module -> Map.HashMap ModuleDigest (NE.NonEmpty MA.ModuleAlias)
getDigestMap baseModule = do
  let depMap = moduleDependency baseModule
  let depList = map (\(alias, dep) -> (dependencyDigest dep, alias)) $ Map.toList depMap
  let groupedDepList = NE.groupBy (\(c1, _) (c2, _) -> c1 == c2) depList
  Map.fromList $ flip map groupedDepList $ \depSummary -> do
    let representative = fst $ NE.head depSummary
    let elems = NE.map snd depSummary
    (representative, elems)

_m :: Hint
_m =
  internalHint

toDefaultEns :: Module -> E.Ens
toDefaultEns someModule =
  E.dictFromList _m $
    catMaybes
      [ return $ getTargetInfo someModule,
        getSourceDirInfo someModule,
        getCacheDirInfo someModule,
        getArchiveDirInfo someModule,
        getExtraContentInfo someModule,
        getForeignInfo someModule,
        getInlineLimitInfo someModule,
        getPrefixMapInfo someModule,
        getPresetMapInfo someModule,
        getAntecedentInfo someModule,
        getDependencyInfo someModule
      ]

getArchiveDirInfo :: Module -> Maybe (T.Text, E.Ens)
getArchiveDirInfo someModule = do
  let dir = moduleArchiveDir someModule
  if dir == archiveRelDir
    then Nothing
    else return (keyArchive, _m :< E.ensPath dir)

getSourceDirInfo :: Module -> Maybe (T.Text, E.Ens)
getSourceDirInfo someModule = do
  let dir = moduleSourceDir someModule
  if dir == sourceRelDir
    then Nothing
    else return (keySource, _m :< E.ensPath dir)

getCacheDirInfo :: Module -> Maybe (T.Text, E.Ens)
getCacheDirInfo someModule = do
  let dir = moduleCacheDir someModule
  if dir == cacheRelDir
    then Nothing
    else return (keyCache, _m :< E.ensPath dir)

getTargetInfo :: Module -> (T.Text, E.Ens)
getTargetInfo someModule = do
  let targetDict = flip Map.map (moduleTarget someModule) $ \summary -> do
        let compileOption = map (\x -> _m :< E.String x) $ CL.compileOption (Target.clangOption summary)
        let compileOption' =
              if null compileOption
                then Nothing
                else Just (keyCompileOption, _m :< E.List (seriesFromList compileOption))
        let linkOption = map (\x -> _m :< E.String x) $ CL.linkOption (Target.clangOption summary)
        let linkOption' =
              if null linkOption
                then Nothing
                else Just (keyLinkOption, _m :< E.List (seriesFromList linkOption))
        E.dictFromListVertical
          _m
          $ [(keyMain, _m :< E.String (SL.getRelPathText (Target.entryPoint summary)))]
            ++ maybeToList compileOption'
            ++ maybeToList linkOption'
  (keyTarget, E.dictFromListVertical _m (Map.toList targetDict))

getDependencyInfo :: Module -> Maybe (T.Text, E.Ens)
getDependencyInfo someModule = do
  let dependency = flip Map.map (moduleDependency someModule) $ \dep -> do
        let urlList = dependencyMirrorList dep
        let ModuleDigest digest = dependencyDigest dep
        let urlEnsList = map (\(ModuleURL url) -> _m :< E.String url) urlList
        let digestEns = _m :< E.String digest
        let mirrorEns = _m :< E.List (seriesFromList urlEnsList)
        E.dictFromListVertical _m [(keyDigest, digestEns), (keyMirror, mirrorEns)]
  let dependency' = Map.mapKeys (\(MA.ModuleAlias key) -> BN.reify key) dependency
  if Map.null dependency'
    then Nothing
    else return (keyDependency, E.dictFromListVertical _m (Map.toList dependency'))

getExtraContentInfo :: Module -> Maybe (T.Text, E.Ens)
getExtraContentInfo someModule = do
  let extraContentList = map (\x -> _m :< E.String (ppExtraContent x)) $ moduleExtraContents someModule
  if null extraContentList
    then Nothing
    else return (keyExtraContent, _m :< E.List (seriesFromList extraContentList))

getAntecedentInfo :: Module -> Maybe (T.Text, E.Ens)
getAntecedentInfo someModule = do
  let antecedentList = map (\x -> _m :< E.String (ppAntecedent x)) $ moduleAntecedents someModule
  if null antecedentList
    then Nothing
    else return (keyAntecedent, _m :< E.List (seriesFromList antecedentList))

getForeignInfo :: Module -> Maybe (T.Text, E.Ens)
getForeignInfo someModule = do
  let foreignInfo = moduleForeign someModule
  let assetList = map (\x -> _m :< E.String (ppExtraContent x)) $ input foreignInfo
  let outputList = map (\x -> _m :< E.String (T.pack $ toFilePath x)) $ output foreignInfo
  let cmdList = map (\x -> _m :< E.String x) $ script foreignInfo
  if null (input foreignInfo) && null (script foreignInfo)
    then Nothing
    else
      return
        ( keyForeign,
          _m
            :< E.dictFromListVertical'
              [ (keyForeignInput, _m :< E.List (seriesFromList assetList)),
                (keyForeignOutput, _m :< E.List (seriesFromList outputList)),
                (keyForeignScript, _m :< E.List (seriesFromList cmdList))
              ]
        )

getPrefixMapInfo :: Module -> Maybe (T.Text, E.Ens)
getPrefixMapInfo someModule = do
  if Map.null (modulePrefixMap someModule)
    then Nothing
    else do
      let prefixMapDict = flip Map.map (modulePrefixMap someModule) $ \(alias, locator) ->
            _m :< E.String (GL.reify (GL.GlobalLocator $ GL.IdentifiedGlobalLocator alias locator))
      let prefixMapDict' = Map.mapKeys BN.reify prefixMapDict
      return (keyPrefix, E.dictFromList _m (Map.toList prefixMapDict'))

getPresetMapInfo :: Module -> Maybe (T.Text, E.Ens)
getPresetMapInfo someModule = do
  if Map.null (modulePresetMap someModule)
    then Nothing
    else do
      let f bns = _m :< E.List (seriesFromList $ map (\bn -> _m :< E.String (BN.reify bn)) $ sort bns)
      return (keyPreset, E.dictFromList _m (Map.toList (Map.map f (modulePresetMap someModule))))

getInlineLimitInfo :: Module -> Maybe (T.Text, E.Ens)
getInlineLimitInfo someModule = do
  limit <- moduleInlineLimit someModule
  return (keyInlineLimit, _m :< E.Int limit)

ppAntecedent :: ModuleDigest -> T.Text
ppAntecedent (ModuleDigest digest) =
  digest

ppExtraContent :: SomePath a -> T.Text
ppExtraContent somePath =
  case somePath of
    Left dirPath ->
      T.pack $ toFilePath dirPath
    Right filePath ->
      T.pack $ toFilePath filePath

ppDirPath :: Path Rel Dir -> T.Text
ppDirPath dirPath =
  T.pack $ toFilePath dirPath

getDigestFromModulePath :: Path Abs File -> MID.ModuleID
getDigestFromModulePath moduleFilePath =
  MID.Library $
    ModuleDigest $
      T.pack $
        FP.dropTrailingPathSeparator $
          toFilePath $
            dirname $
              parent moduleFilePath

getTarget :: Module -> T.Text -> Maybe Target.MainTarget
getTarget someModule targetName = do
  target <- Map.lookup targetName (moduleTarget someModule)
  return $ Target.Named targetName target

stylize :: E.Ens -> Either Error E.Ens
stylize ens = do
  if not $ E.hasKey keyDependency ens
    then return ens
    else do
      (m, depDict) <- E.access keyDependency ens >>= E.toDictionary
      depDict' <- forM depDict $ \(k, dep) -> do
        (mDep, mirrorList) <- E.access keyMirror dep >>= E.toList
        let mirrorList' = SE.nubSeriesBy (\e1 e2 -> E.EqEns e1 == E.EqEns e2) mirrorList
        dep' <- E.put keyMirror (mDep :< E.List mirrorList') dep
        return (k, dep')
      E.put keyDependency (m :< E.Dictionary depDict') ens

getAliasListWithEnabledPresets :: Module -> [MA.ModuleAlias]
getAliasListWithEnabledPresets baseModule = do
  let depList = Map.toList $ moduleDependency baseModule
  map fst $ filter (\(_, dep) -> dependencyPresetEnabled dep) depList

type PresetSummary =
  [(T.Text, [BN.BaseName])]

reifyPresetMap :: T.Text -> PresetMap -> PresetSummary
reifyPresetMap moduleName presetMap = do
  let presetList = Map.toList presetMap
  flip map presetList $ \(loc, lls) -> do
    (moduleName <> nsSep <> loc, lls)

getRelPathFromSourceDir :: (MonadThrow m) => Module -> Path Abs File -> m (Path Rel File)
getRelPathFromSourceDir baseModule path = do
  let sourceDir = getSourceDir baseModule
  stripProperPrefix sourceDir path

seriesFromList :: [a] -> SE.Series a
seriesFromList =
  SE.fromList SE.Bracket SE.Comma
