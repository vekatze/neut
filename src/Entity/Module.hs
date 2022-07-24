module Entity.Module where

import Context.Throw
import Control.Comonad.Cofree
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import qualified Entity.BaseName as BN
import Entity.Const
import Entity.Ens
import Entity.ModuleAlias
import Entity.ModuleChecksum
import qualified Entity.ModuleID as MID
import Entity.ModuleURL
import qualified Entity.StrictGlobalLocator as SGL
import qualified Entity.Target as Target
import Path
import Path.IO
import qualified System.FilePath as FP

type SomePath =
  Either (Path Abs Dir) (Path Abs File)

data Module = Module
  { moduleTarget :: Map.HashMap Target.Target SGL.StrictGlobalLocator,
    moduleDependency :: Map.HashMap ModuleAlias (ModuleURL, ModuleChecksum),
    moduleExtraContents :: [SomePath],
    moduleLocation :: Path Abs File
  }
  deriving (Show)

getSourceDir :: Module -> Path Abs Dir
getSourceDir baseModule =
  getModuleRootDir baseModule </> sourceRelDir

getTargetDir :: Module -> Path Abs Dir
getTargetDir baseModule =
  getModuleRootDir baseModule </> targetRelDir

getReleaseDir :: Module -> Path Abs Dir
getReleaseDir baseModule =
  getModuleRootDir baseModule </> releaseRelDir

getArtifactDir :: Module -> Path Abs Dir
getArtifactDir baseModule =
  getTargetDir baseModule </> artifactRelDir

getExecutableDir :: Module -> Path Abs Dir
getExecutableDir baseModule =
  getTargetDir baseModule </> executableRelDir

getExecutableOutputPath :: Target.Target -> Module -> IO (Path Abs File)
getExecutableOutputPath target mainModule =
  resolveFile (getExecutableDir mainModule) $ T.unpack $ Target.extract target

getModuleRootDir :: Module -> Path Abs Dir
getModuleRootDir baseModule =
  parent $ moduleLocation baseModule

findModuleFile :: Context -> Path Abs Dir -> IO (Path Abs File)
findModuleFile ctx moduleRootDirCandidate = do
  let moduleFileCandidate = moduleRootDirCandidate </> moduleFile
  moduleFileExists <- doesFileExist moduleFileCandidate
  case (moduleFileExists, moduleRootDirCandidate /= parent moduleRootDirCandidate) of
    (True, _) ->
      return moduleFileCandidate
    (_, True) ->
      findModuleFile ctx $ parent moduleRootDirCandidate
    _ ->
      raiseError' ctx "couldn't find a module file."

getCurrentModuleFilePath :: Context -> IO (Path Abs File)
getCurrentModuleFilePath ctx =
  getCurrentDir >>= findModuleFile ctx

addDependency :: ModuleAlias -> ModuleURL -> ModuleChecksum -> Module -> Module
addDependency alias url checksum someModule =
  someModule {moduleDependency = Map.insert alias (url, checksum) (moduleDependency someModule)}

ppModule :: Module -> T.Text
ppModule someModule = do
  let entryPoint = Map.map (\x -> () :< EnsString (SGL.getRelPathText x)) $ moduleTarget someModule
  let dependency = flip Map.map (moduleDependency someModule) $ \(ModuleURL url, ModuleChecksum checksum) -> do
        let urlEns = () :< EnsString url
        let checksumEns = () :< EnsString checksum
        () :< EnsDictionary (Map.fromList [("checksum", checksumEns), ("URL", urlEns)])
  let extraContents = map (\x -> () :< EnsString (ppExtraContent x)) $ moduleExtraContents someModule
  ppEnsTopLevel $
    Map.fromList
      [ ("dependency", () :< EnsDictionary (Map.mapKeys (\(ModuleAlias key) -> BN.reify key) dependency)),
        ("target", () :< EnsDictionary (Map.mapKeys (\(Target.Target key) -> key) entryPoint)),
        ("extra-content", () :< EnsList extraContents)
      ]

ppExtraContent :: SomePath -> T.Text
ppExtraContent somePath =
  case somePath of
    Left dirPath ->
      T.pack $ toFilePath dirPath
    Right filePath ->
      T.pack $ toFilePath filePath

getID :: Module -> Module -> MID.ModuleID
getID mainModule currentModule = do
  if moduleLocation mainModule == moduleLocation currentModule
    then MID.Main
    else
      MID.Library $
        ModuleChecksum $
          T.pack $ FP.dropTrailingPathSeparator $ toFilePath $ dirname $ parent (moduleLocation currentModule)
