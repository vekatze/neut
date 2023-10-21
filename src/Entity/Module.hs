module Entity.Module where

import Control.Comonad.Cofree
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.Const
import Entity.Ens qualified as E
import Entity.GlobalLocator qualified as GL
import Entity.ModuleAlias
import Entity.ModuleDigest
import Entity.ModuleID qualified as MID
import Entity.ModuleURL
import Entity.SourceLocator qualified as SL
import Entity.Target qualified as Target
import Path
import System.FilePath qualified as FP

type SomePath a =
  Either (Path a Dir) (Path a File)

data Module = Module
  { moduleID :: MID.ModuleID,
    moduleSourceDir :: Path Rel Dir,
    moduleTarget :: Map.HashMap Target.Target SL.SourceLocator,
    moduleArchiveDir :: Path Rel Dir,
    moduleBuildDir :: Path Rel Dir,
    moduleDependency :: Map.HashMap ModuleAlias ([ModuleURL], ModuleDigest),
    moduleExtraContents :: [SomePath Rel],
    moduleAntecedents :: [ModuleDigest],
    moduleLocation :: Path Abs File,
    moduleForeignDirList :: [Path Rel Dir],
    modulePrefixMap :: Map.HashMap BN.BaseName (ModuleAlias, SL.SourceLocator)
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

addDependency :: ModuleAlias -> [ModuleURL] -> ModuleDigest -> Module -> Module
addDependency alias mirrorList digest someModule = do
  someModule {moduleDependency = Map.insert alias (mirrorList, digest) (moduleDependency someModule)}

insertDependency :: ([ModuleURL], [ModuleDigest]) -> ([ModuleURL], [ModuleDigest]) -> ([ModuleURL], [ModuleDigest])
insertDependency (mirrorList1, digest1) (mirrorList2, _) = do
  (nubOrd $ mirrorList1 ++ mirrorList2, digest1)

getDigestMap :: Module -> Map.HashMap ModuleDigest (NE.NonEmpty ModuleAlias)
getDigestMap baseModule = do
  let dep = moduleDependency baseModule
  let foo = map (\(alias, (_, digest)) -> (digest, alias)) $ Map.toList dep
  let groupedFoo = NE.groupBy (\(c1, _) (c2, _) -> c1 == c2) foo
  Map.fromList $ flip map groupedFoo $ \item -> do
    let representative = fst $ NE.head item
    let elems = NE.map snd item
    (representative, elems)

ppModule :: Module -> T.Text
ppModule someModule = do
  E.ppEnsTopLevel $
    ()
      :< E.Dictionary
        ( Map.fromList $
            catMaybes
              [ getSourceDirInfo someModule,
                getBuildDirInfo someModule,
                getArchiveDirInfo someModule,
                return $ getTargetInfo someModule,
                getExtraContentInfo someModule,
                getForeignInfo someModule,
                getAntecedentInfo someModule,
                getDependencyInfo someModule,
                getPrefixMapInfo someModule
              ]
        )

getArchiveDirInfo :: Module -> Maybe (T.Text, E.MiniEns)
getArchiveDirInfo someModule = do
  let dir = moduleArchiveDir someModule
  if dir == archiveRelDir
    then Nothing
    else return (keyArchive, () :< E.ensPath dir)

getSourceDirInfo :: Module -> Maybe (T.Text, E.MiniEns)
getSourceDirInfo someModule = do
  let dir = moduleSourceDir someModule
  if dir == sourceRelDir
    then Nothing
    else return (keySource, () :< E.ensPath dir)

getBuildDirInfo :: Module -> Maybe (T.Text, E.MiniEns)
getBuildDirInfo someModule = do
  let dir = moduleBuildDir someModule
  if dir == buildRelDir
    then Nothing
    else return (keyBuild, () :< E.ensPath dir)

getTargetInfo :: Module -> (T.Text, E.MiniEns)
getTargetInfo someModule = do
  let targetDict = Map.map (\x -> () :< E.String (SL.getRelPathText x)) $ moduleTarget someModule
  let targetDict' = Map.mapKeys (\(Target.Target key) -> key) targetDict
  (keyTarget, () :< E.Dictionary targetDict')

getDependencyInfo :: Module -> Maybe (T.Text, E.MiniEns)
getDependencyInfo someModule = do
  let dependency = flip Map.map (moduleDependency someModule) $ \(urlList, ModuleDigest digest) -> do
        let urlEnsList = map (\(ModuleURL url) -> () :< E.String url) urlList
        let digestEns = () :< E.String digest
        () :< E.Dictionary (Map.fromList [("digest", digestEns), ("mirror", () :< E.List urlEnsList)])
  let dependency' = Map.mapKeys (\(ModuleAlias key) -> BN.reify key) dependency
  if Map.null dependency'
    then Nothing
    else return (keyDependency, () :< E.Dictionary dependency')

getExtraContentInfo :: Module -> Maybe (T.Text, E.MiniEns)
getExtraContentInfo someModule = do
  let extraContentList = map (\x -> () :< E.String (ppExtraContent x)) $ moduleExtraContents someModule
  if null extraContentList
    then Nothing
    else return (keyExtraContent, () :< E.List extraContentList)

getAntecedentInfo :: Module -> Maybe (T.Text, E.MiniEns)
getAntecedentInfo someModule = do
  let antecedentList = map (\x -> () :< E.String (ppAntecedent x)) $ moduleAntecedents someModule
  if null antecedentList
    then Nothing
    else return (keyAntecedent, () :< E.List antecedentList)

getForeignInfo :: Module -> Maybe (T.Text, E.MiniEns)
getForeignInfo someModule = do
  let foreignList = map (\x -> () :< E.String (ppDirPath x)) $ moduleForeignDirList someModule
  if null foreignList
    then Nothing
    else return (keyForeign, () :< E.List foreignList)

getPrefixMapInfo :: Module -> Maybe (T.Text, E.MiniEns)
getPrefixMapInfo someModule = do
  if Map.null (modulePrefixMap someModule)
    then Nothing
    else do
      let prefixMapDict = flip Map.map (modulePrefixMap someModule) $ \(alias, locator) ->
            () :< E.String (GL.reify (GL.GlobalLocator alias locator))
      let prefixMapDict' = Map.mapKeys BN.reify prefixMapDict
      return (keyPrefix, () :< E.Dictionary prefixMapDict')

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

getID :: Module -> Module -> MID.ModuleID
getID mainModule currentModule = do
  if moduleLocation mainModule == moduleLocation currentModule
    then MID.Main
    else getDigestFromModulePath (moduleLocation currentModule)

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
