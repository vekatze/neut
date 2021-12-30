{-# LANGUAGE TemplateHaskell #-}

module Data.Module where

import Control.Comonad.Cofree (Cofree (..))
import Data.Basic (Alias, Checksum (..), URL (..))
import Data.Entity (EntityF (EntityDictionary, EntityString), ppEntityTopLevel)
import qualified Data.HashMap.Lazy as Map
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Log (raiseCritical', raiseError')
import qualified Data.Text as T
import Path (Abs, Dir, File, Path, Rel, mkRelDir, mkRelFile, parent, toFilePath, (</>))
import Path.IO (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)

data Module = Module
  { moduleTarget :: Map.HashMap T.Text (Path Rel File),
    moduleDependency :: Map.HashMap Alias (URL, Checksum),
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

getChecksumAliasList :: Module -> [(T.Text, T.Text)]
getChecksumAliasList baseModule = do
  let dependencyList = Map.toList $ moduleDependency baseModule
  map (\(key, (_, Checksum checksum)) -> (key, checksum)) dependencyList

findModuleFile :: Path Abs Dir -> IO (Path Abs File)
findModuleFile moduleRootDirCandidate = do
  let moduleFileCandidate = moduleRootDirCandidate </> moduleFile
  moduleFileExists <- doesFileExist moduleFileCandidate
  case (moduleFileExists, moduleRootDirCandidate /= parent moduleRootDirCandidate) of
    (True, _) ->
      return moduleFileCandidate
    (_, True) ->
      findModuleFile $ parent moduleRootDirCandidate
    _ ->
      raiseError' "could not find a module file."

{-# NOINLINE mainModuleRef #-}
mainModuleRef :: IORef (Maybe Module)
mainModuleRef =
  unsafePerformIO (newIORef Nothing)

setMainModule :: Module -> IO ()
setMainModule mainModule = do
  mainModuleOrNothing <- readIORef mainModuleRef
  case mainModuleOrNothing of
    Just _ ->
      raiseCritical' "the main module is already initialized"
    Nothing ->
      modifyIORef' mainModuleRef $ const $ Just mainModule

getMainModule :: IO Module
getMainModule = do
  mainModuleOrNothing <- readIORef mainModuleRef
  case mainModuleOrNothing of
    Just mainModule ->
      return mainModule
    Nothing ->
      raiseCritical' "the main module is not initialized"

addDependency :: Alias -> URL -> Checksum -> Module -> Module
addDependency alias url checksum someModule =
  someModule {moduleDependency = Map.insert alias (url, checksum) (moduleDependency someModule)}

ppModule :: Module -> T.Text
ppModule someModule = do
  let entryPoint = Map.map (\x -> () :< EntityString (T.pack (toFilePath x))) $ moduleTarget someModule
  let dependency = flip Map.map (moduleDependency someModule) $ \(URL url, Checksum checksum) -> do
        let urlEntity = () :< EntityString url
        let checksumEntity = () :< EntityString checksum
        () :< EntityDictionary (Map.fromList [("checksum", checksumEntity), ("URL", urlEntity)])
  ppEntityTopLevel $
    Map.fromList
      [ ("dependency", () :< EntityDictionary dependency),
        ("target", () :< EntityDictionary entryPoint)
      ]
