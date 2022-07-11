{-# LANGUAGE TemplateHaskell #-}

module Entity.Module where

import Context.Throw
import Control.Comonad.Cofree
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import Entity.Ens
import Entity.ModuleAlias
import Entity.ModuleChecksum
import Entity.ModuleURL
import Entity.Target
import Path
import Path.IO

type SomePath =
  Either (Path Abs Dir) (Path Abs File)

data Module = Module
  { moduleTarget :: Map.HashMap Target (Path Rel File),
    moduleDependency :: Map.HashMap ModuleAlias (ModuleURL, ModuleChecksum),
    moduleExtraContents :: [SomePath],
    moduleLocation :: Path Abs File
  }
  deriving (Show)

moduleFile :: Path Rel File
moduleFile =
  $(mkRelFile "module.ens")

defaultModulePrefix :: T.Text
defaultModulePrefix =
  "this"

getSourceDir :: Module -> Path Abs Dir
getSourceDir baseModule =
  parent (moduleLocation baseModule) </> $(mkRelDir "source")

getTargetDir :: Module -> Path Abs Dir
getTargetDir baseModule =
  parent (moduleLocation baseModule) </> $(mkRelDir "target")

getReleaseDir :: Module -> Path Abs Dir
getReleaseDir baseModule =
  getModuleRootDir baseModule </> $(mkRelDir "release")

getArtifactDir :: Module -> Path Abs Dir
getArtifactDir baseModule =
  getTargetDir baseModule </> $(mkRelDir "artifact")

getExecutableDir :: Module -> Path Abs Dir
getExecutableDir baseModule =
  getTargetDir baseModule </> $(mkRelDir "executable")

getModuleRootDir :: Module -> Path Abs Dir
getModuleRootDir baseModule =
  parent $ moduleLocation baseModule

getTargetFilePath :: Module -> Target -> Maybe (Path Abs File)
getTargetFilePath baseModule target = do
  relPath <- Map.lookup target (moduleTarget baseModule)
  return $ getSourceDir baseModule </> relPath

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

getMainModuleFilePath :: Context -> IO (Path Abs File)
getMainModuleFilePath ctx =
  getCurrentDir >>= findModuleFile ctx

getCurrentModuleFilePath :: Context -> IO (Path Abs File)
getCurrentModuleFilePath ctx =
  getCurrentDir >>= findModuleFile ctx

addDependency :: ModuleAlias -> ModuleURL -> ModuleChecksum -> Module -> Module
addDependency alias url checksum someModule =
  someModule {moduleDependency = Map.insert alias (url, checksum) (moduleDependency someModule)}

ppModule :: Module -> T.Text
ppModule someModule = do
  let entryPoint = Map.map (\x -> () :< EnsString (T.pack (toFilePath x))) $ moduleTarget someModule
  let dependency = flip Map.map (moduleDependency someModule) $ \(ModuleURL url, ModuleChecksum checksum) -> do
        let urlEns = () :< EnsString url
        let checksumEns = () :< EnsString checksum
        () :< EnsDictionary (Map.fromList [("checksum", checksumEns), ("URL", urlEns)])
  let extraContents = map (\x -> () :< EnsString (ppExtraContent x)) $ moduleExtraContents someModule
  ppEnsTopLevel $
    Map.fromList
      [ ("dependency", () :< EnsDictionary (Map.mapKeys (\(ModuleAlias key) -> key) dependency)),
        ("target", () :< EnsDictionary (Map.mapKeys (\(Target key) -> key) entryPoint)),
        ("extra-content", () :< EnsList extraContents)
      ]

ppExtraContent :: SomePath -> T.Text
ppExtraContent somePath =
  case somePath of
    Left dirPath ->
      T.pack $ toFilePath dirPath
    Right filePath ->
      T.pack $ toFilePath filePath
