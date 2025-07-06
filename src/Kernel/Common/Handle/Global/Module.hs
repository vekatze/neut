module Kernel.Common.Handle.Global.Module
  ( Handle (..),
    new,
    getModuleFilePath,
    getModuleDirByID,
    getModuleCacheMap,
    getCoreModuleURL,
    getCoreModuleDigest,
    insertToModuleCacheMap,
    sourceFromPath,
    getAllSourcePathInModule,
    getAllSourceInModule,
    _hasSourceExtension,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Error.EIO (EIO)
import Error.Run (raiseError, raiseError')
import Kernel.Common.Const
import Kernel.Common.Module
import Kernel.Common.ModuleURL
import Kernel.Common.Source qualified as Source
import Language.Common.ModuleDigest
import Language.Common.ModuleDigest qualified as MD
import Language.Common.ModuleID qualified as MID
import Logger.Hint qualified as H
import Path
import Path.IO
import System.Environment

newtype Handle = Handle
  { _moduleCacheMapRef :: IORef (Map.HashMap (Path Abs File) Module)
  }

_hasSourceExtension :: Path Abs File -> Bool
_hasSourceExtension path =
  case splitExtension path of
    Just (_, ext)
      | ext == sourceFileExtension ->
          True
    _ ->
      False

new :: IO Handle
new = do
  _moduleCacheMapRef <- newIORef Map.empty
  return $ Handle {..}

getModuleFilePath :: MainModule -> Maybe H.Hint -> MID.ModuleID -> EIO (Path Abs File)
getModuleFilePath mainModule mHint moduleID = do
  moduleDir <- getModuleDirByID mainModule mHint moduleID
  return $ moduleDir </> moduleFile

getModuleCacheMap :: Handle -> IO (Map.HashMap (Path Abs File) Module)
getModuleCacheMap h =
  readIORef (_moduleCacheMapRef h)

insertToModuleCacheMap :: Handle -> Path Abs File -> Module -> IO ()
insertToModuleCacheMap h k v =
  atomicModifyIORef' (_moduleCacheMapRef h) (\mp -> (Map.insert k v mp, ()))

getModuleDirByID :: MainModule -> Maybe H.Hint -> MID.ModuleID -> EIO (Path Abs Dir)
getModuleDirByID (MainModule pivotModule) mHint moduleID = do
  case moduleID of
    MID.Base -> do
      let message = "The base module cannot be used here"
      case mHint of
        Just hint ->
          raiseError hint message
        Nothing ->
          raiseError' message
    MID.Main ->
      return $ getModuleRootDir pivotModule
    MID.Library (MD.ModuleDigest digest) -> do
      dependencyDir <- getDependencyDirPath pivotModule
      resolveDir dependencyDir $ T.unpack digest

getCoreModuleURL :: EIO ModuleURL
getCoreModuleURL = do
  mCoreModuleURL <- liftIO $ lookupEnv envVarCoreModuleURL
  case mCoreModuleURL of
    Just coreModuleURL ->
      return $ ModuleURL $ T.pack coreModuleURL
    Nothing ->
      raiseError' $ "The URL of the core module is not specified; set it via " <> T.pack envVarCoreModuleURL

getCoreModuleDigest :: EIO ModuleDigest
getCoreModuleDigest = do
  mCoreModuleDigest <- liftIO $ lookupEnv envVarCoreModuleDigest
  case mCoreModuleDigest of
    Just coreModuleDigest ->
      return $ ModuleDigest $ T.pack coreModuleDigest
    Nothing ->
      raiseError' $ "The digest of the core module is not specified; set it via " <> T.pack envVarCoreModuleDigest

sourceFromPath :: Module -> Path Abs File -> EIO Source.Source
sourceFromPath baseModule path = do
  ensureFileModuleSanity path baseModule
  return $
    Source.Source
      { Source.sourceModule = baseModule,
        Source.sourceFilePath = path,
        Source.sourceHint = Nothing
      }

ensureFileModuleSanity :: Path Abs File -> Module -> EIO ()
ensureFileModuleSanity filePath mainModule = do
  unless (isProperPrefixOf (getSourceDir mainModule) filePath) $ do
    raiseError' $
      "The file `"
        <> T.pack (toFilePath filePath)
        <> "` is not in the source directory of current module"

getAllSourcePathInModule :: Module -> EIO [Path Abs File]
getAllSourcePathInModule baseModule = do
  (_, filePathList) <- listDirRecur (getSourceDir baseModule)
  return $ filter _hasSourceExtension filePathList

getAllSourceInModule :: Module -> EIO [Source.Source]
getAllSourceInModule baseModule = do
  sourcePathList <- getAllSourcePathInModule baseModule
  mapM (sourceFromPath baseModule) sourcePathList

getDependencyDirPath :: Module -> EIO (Path Abs Dir)
getDependencyDirPath baseModule = do
  let moduleRootDir = getModuleRootDir baseModule
  case moduleID baseModule of
    MID.Library _ ->
      returnDirectory $ parent moduleRootDir
    _ -> do
      returnDirectory $ moduleRootDir </> moduleCacheDir baseModule </> $(mkRelDir "dependency")

returnDirectory :: Path Abs Dir -> EIO (Path Abs Dir)
returnDirectory path =
  ensureDir path >> return path
