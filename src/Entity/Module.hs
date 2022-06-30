{-# LANGUAGE TemplateHaskell #-}

module Entity.Module where

import Context.Throw
import Control.Comonad.Cofree
import Data.Function
import qualified Data.HashMap.Lazy as Map
import Data.IORef
import qualified Data.Text as T
import Entity.Ens
import Entity.ModuleAlias
import Entity.ModuleChecksum
import Entity.ModuleURL
import Path
import Path.IO
import System.IO.Unsafe

type SomePath =
  Either (Path Abs Dir) (Path Abs File)

data Module = Module
  { moduleTarget :: Map.HashMap T.Text (Path Rel File),
    moduleDependency :: Map.HashMap T.Text (ModuleURL, ModuleChecksum),
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

getTargetFilePath :: Module -> T.Text -> Maybe (Path Abs File)
getTargetFilePath baseModule target = do
  relPath <- Map.lookup target (moduleTarget baseModule)
  return $ getSourceDir baseModule </> relPath

getModuleChecksumAliasList :: Module -> [(T.Text, T.Text)]
getModuleChecksumAliasList baseModule = do
  let dependencyList = Map.toList $ moduleDependency baseModule
  map (\(key, (_, ModuleChecksum checksum)) -> (key, checksum)) dependencyList

findModuleFile :: Context -> Path Abs Dir -> IO (Path Abs File)
findModuleFile context moduleRootDirCandidate = do
  let moduleFileCandidate = moduleRootDirCandidate </> moduleFile
  moduleFileExists <- doesFileExist moduleFileCandidate
  case (moduleFileExists, moduleRootDirCandidate /= parent moduleRootDirCandidate) of
    (True, _) ->
      return moduleFileCandidate
    (_, True) ->
      findModuleFile context $ parent moduleRootDirCandidate
    _ ->
      context & raiseError' $ "could not find a module file."

getMainModuleFilePath :: Context -> IO (Path Abs File)
getMainModuleFilePath context =
  getCurrentDir >>= findModuleFile context

{-# NOINLINE mainModuleRef #-}
mainModuleRef :: IORef (Maybe Module)
mainModuleRef =
  unsafePerformIO (newIORef Nothing)

setMainModule :: Context -> Module -> IO ()
setMainModule context mainModule = do
  mainModuleOrNothing <- readIORef mainModuleRef
  case mainModuleOrNothing of
    Just _ ->
      context & raiseCritical' $ "the main module is already initialized"
    Nothing ->
      modifyIORef' mainModuleRef $ const $ Just mainModule

getMainModule :: Context -> IO Module
getMainModule context = do
  mainModuleOrNothing <- readIORef mainModuleRef
  case mainModuleOrNothing of
    Just mainModule ->
      return mainModule
    Nothing ->
      (context & raiseCritical') "the main module is not initialized"

addDependency :: ModuleAlias -> ModuleURL -> ModuleChecksum -> Module -> Module
addDependency (ModuleAlias alias) url checksum someModule =
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
      [ ("dependency", () :< EnsDictionary dependency),
        ("target", () :< EnsDictionary entryPoint),
        ("extra-content", () :< EnsList extraContents)
      ]

ppExtraContent :: SomePath -> T.Text
ppExtraContent somePath =
  case somePath of
    Left dirPath ->
      T.pack $ toFilePath dirPath
    Right filePath ->
      T.pack $ toFilePath filePath
