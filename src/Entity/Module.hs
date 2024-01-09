module Entity.Module where

import Control.Comonad.Cofree
import Control.Monad
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict qualified as Map
import Data.List (find, sort)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.Const
import Entity.Ens qualified as E
import Entity.Ens.Reify qualified as Ens
import Entity.Error
import Entity.GlobalLocator qualified as GL
import Entity.Hint (Hint, internalHint)
import Entity.ModuleAlias qualified as MA
import Entity.ModuleDigest
import Entity.ModuleID qualified as MID
import Entity.ModuleURL
import Entity.SourceLocator qualified as SL
import Entity.Target qualified as Target
import Path
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

data Module = Module
  { moduleID :: MID.ModuleID,
    moduleSourceDir :: Path Rel Dir,
    moduleTarget :: Map.HashMap Target.Target SL.SourceLocator,
    moduleArchiveDir :: Path Rel Dir,
    moduleBuildDir :: Path Rel Dir,
    moduleDependency :: Map.HashMap MA.ModuleAlias Dependency,
    moduleExtraContents :: [SomePath Rel],
    moduleAntecedents :: [ModuleDigest],
    moduleLocation :: Path Abs File,
    moduleForeignDirList :: [Path Rel Dir],
    modulePrefixMap :: Map.HashMap BN.BaseName (MA.ModuleAlias, SL.SourceLocator),
    moduleInlineLimit :: Maybe Int,
    modulePresetMap :: PresetMap
  }
  deriving (Show)

keyArchive :: T.Text
keyArchive =
  "archive"

keyBuild :: T.Text
keyBuild =
  "build"

keySource :: T.Text
keySource =
  "source"

keyTarget :: T.Text
keyTarget =
  "target"

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

keyForeign :: T.Text
keyForeign =
  "foreign"

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
  map ((moduleSourceDir </>) . SL.reify) sourceLocatorList

getTargetPath :: Module -> Target.Target -> Maybe (Path Abs File)
getTargetPath baseModule target = do
  let moduleSourceDir = getSourceDir baseModule
  sourceLocator <- Map.lookup target (moduleTarget baseModule)
  return $ moduleSourceDir </> SL.reify sourceLocator

getArchiveDir :: Module -> Path Abs Dir
getArchiveDir baseModule =
  getModuleRootDir baseModule </> moduleArchiveDir baseModule

getForeignContents :: Module -> [Path Abs Dir]
getForeignContents baseModule = do
  let moduleRootDir = getModuleRootDir baseModule
  map (moduleRootDir </>) $ moduleForeignDirList baseModule

getExtraContents :: Module -> [SomePath Abs]
getExtraContents baseModule = do
  let moduleRootDir = getModuleRootDir baseModule
  flip map (moduleExtraContents baseModule) $ \somePath -> do
    case somePath of
      Left dirPath ->
        Left $ moduleRootDir </> dirPath
      Right filePath ->
        Right $ moduleRootDir </> filePath

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

ppModule :: Module -> T.Text
ppModule someModule = do
  Ens.pp ([], (toDefaultEns someModule, []))

toDefaultEns :: Module -> E.Ens
toDefaultEns someModule =
  _m
    :< E.Dictionary
      []
      ( catMaybes
          [ getSourceDirInfo someModule,
            getBuildDirInfo someModule,
            getArchiveDirInfo someModule,
            return $ getTargetInfo someModule,
            getExtraContentInfo someModule,
            getForeignInfo someModule,
            getAntecedentInfo someModule,
            getDependencyInfo someModule,
            getPrefixMapInfo someModule,
            getInlineLimitInfo someModule,
            getPresetMapInfo someModule
          ]
      )

getArchiveDirInfo :: Module -> Maybe (T.Text, E.FullEns)
getArchiveDirInfo someModule = do
  let dir = moduleArchiveDir someModule
  if dir == archiveRelDir
    then Nothing
    else return (keyArchive, E.inject $ _m :< E.ensPath dir)

getSourceDirInfo :: Module -> Maybe (T.Text, E.FullEns)
getSourceDirInfo someModule = do
  let dir = moduleSourceDir someModule
  if dir == sourceRelDir
    then Nothing
    else return (keySource, E.inject $ _m :< E.ensPath dir)

getBuildDirInfo :: Module -> Maybe (T.Text, E.FullEns)
getBuildDirInfo someModule = do
  let dir = moduleBuildDir someModule
  if dir == buildRelDir
    then Nothing
    else return (keyBuild, E.inject $ _m :< E.ensPath dir)

getTargetInfo :: Module -> (T.Text, E.FullEns)
getTargetInfo someModule = do
  let targetDict = Map.map (\x -> E.inject $ _m :< E.String (SL.getRelPathText x)) $ moduleTarget someModule
  let targetDict' = Map.mapKeys (\(Target.Target key) -> key) targetDict
  (keyTarget, E.inject $ _m :< E.Dictionary [] (Map.toList targetDict'))

getDependencyInfo :: Module -> Maybe (T.Text, E.FullEns)
getDependencyInfo someModule = do
  let dependency = flip Map.map (moduleDependency someModule) $ \dep -> do
        let urlList = dependencyMirrorList dep
        let ModuleDigest digest = dependencyDigest dep
        let urlEnsList = map (\(ModuleURL url) -> (_m :< E.String url, [])) urlList
        let digestEns = E.inject $ _m :< E.String digest
        let mirrorEns = E.inject $ _m :< E.List [] urlEnsList
        E.inject $ _m :< E.Dictionary [] [(keyDigest, digestEns), (keyMirror, mirrorEns)]
  let dependency' = Map.mapKeys (\(MA.ModuleAlias key) -> BN.reify key) dependency
  if Map.null dependency'
    then Nothing
    else return (keyDependency, E.inject $ _m :< E.Dictionary [] (Map.toList dependency'))

getExtraContentInfo :: Module -> Maybe (T.Text, E.FullEns)
getExtraContentInfo someModule = do
  let extraContentList = map (\x -> (_m :< E.String (ppExtraContent x), [])) $ moduleExtraContents someModule
  if null extraContentList
    then Nothing
    else return (keyExtraContent, E.inject $ _m :< E.List [] extraContentList)

getAntecedentInfo :: Module -> Maybe (T.Text, E.FullEns)
getAntecedentInfo someModule = do
  let antecedentList = map (\x -> (_m :< E.String (ppAntecedent x), [])) $ moduleAntecedents someModule
  if null antecedentList
    then Nothing
    else return (keyAntecedent, E.inject $ _m :< E.List [] antecedentList)

getForeignInfo :: Module -> Maybe (T.Text, E.FullEns)
getForeignInfo someModule = do
  let foreignList = map (\x -> (_m :< E.String (ppDirPath x), [])) $ moduleForeignDirList someModule
  if null foreignList
    then Nothing
    else return (keyForeign, E.inject $ _m :< E.List [] foreignList)

getPrefixMapInfo :: Module -> Maybe (T.Text, E.FullEns)
getPrefixMapInfo someModule = do
  if Map.null (modulePrefixMap someModule)
    then Nothing
    else do
      let prefixMapDict = flip Map.map (modulePrefixMap someModule) $ \(alias, locator) ->
            E.inject $ _m :< E.String (GL.reify (GL.GlobalLocator alias locator))
      let prefixMapDict' = Map.mapKeys BN.reify prefixMapDict
      return (keyPrefix, E.inject $ _m :< E.Dictionary [] (Map.toList prefixMapDict'))

getPresetMapInfo :: Module -> Maybe (T.Text, E.FullEns)
getPresetMapInfo someModule = do
  if Map.null (modulePresetMap someModule)
    then Nothing
    else do
      let f bns = E.inject $ _m :< E.List [] (map (\bn -> (_m :< E.String (BN.reify bn), [])) $ sort bns)
      return (keyPreset, E.inject $ _m :< E.Dictionary [] (Map.toList (Map.map f (modulePresetMap someModule))))

getInlineLimitInfo :: Module -> Maybe (T.Text, E.FullEns)
getInlineLimitInfo someModule = do
  limit <- moduleInlineLimit someModule
  return (keyInlineLimit, E.inject $ _m :< E.Int limit)

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

getTargetList :: Module -> Maybe Target.Target -> [Target.Target]
getTargetList someModule mTarget =
  case mTarget of
    Just target ->
      [target]
    Nothing -> do
      Map.keys $ moduleTarget someModule

stylize :: E.Ens -> Either Error E.Ens
stylize ens = do
  if not $ E.hasKey keyDependency ens
    then return ens
    else do
      (m, c, depDict) <- E.access keyDependency ens >>= E.toDictionary . E.strip
      depDict' <- forM depDict $ \(k, (c1, (dep, c2))) -> do
        (mDep, cList, mirrorList) <- E.access keyMirror dep >>= E.toList . E.strip
        dep' <- E.put keyMirror (mDep :< E.List cList (E.nubEnsList mirrorList)) dep
        return (k, (c1, (dep', c2)))
      E.put keyDependency (m :< E.Dictionary c depDict') ens

getReadableModuleID :: Module -> MID.ModuleID -> Maybe T.Text
getReadableModuleID baseModule mid =
  case mid of
    MID.Main ->
      Just "this"
    MID.Base ->
      Just "base"
    MID.Library digest -> do
      let depMap = Map.toList $ moduleDependency baseModule
      fmap (MA.reify . fst) $ flip find depMap $ \(_, dep) -> digest == dependencyDigest dep

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
