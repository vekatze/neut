module Move.Context.Module
  ( Handle,
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
  )
where

import Control.Monad
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.EIO (EIO, raiseError')
import Move.Context.Path qualified as Path
import Path
import Path.IO
import Rule.Const
import Rule.Error (newError, newError')
import Rule.Hint qualified as H
import Rule.Module
import Rule.ModuleDigest
import Rule.ModuleDigest qualified as MD
import Rule.ModuleID qualified as MID
import Rule.ModuleURL
import Rule.Source qualified as Source
import System.Environment

newtype Handle
  = Handle
  { moduleCacheMapRef :: IORef (Map.HashMap (Path Abs File) Module)
  }

new :: App Handle
new = do
  moduleCacheMapRef <- asks App.moduleCacheMap
  return $ Handle {..}

getModuleFilePath :: MainModule -> Maybe H.Hint -> MID.ModuleID -> EIO (Path Abs File)
getModuleFilePath mainModule mHint moduleID = do
  moduleDir <- getModuleDirByID mainModule mHint moduleID
  return $ moduleDir </> moduleFile

getModuleCacheMap :: Handle -> IO (Map.HashMap (Path Abs File) Module)
getModuleCacheMap h =
  readIORef (moduleCacheMapRef h)

insertToModuleCacheMap :: Handle -> Path Abs File -> Module -> IO ()
insertToModuleCacheMap h k v =
  modifyIORef' (moduleCacheMapRef h) $ Map.insert k v

getModuleDirByID :: MainModule -> Maybe H.Hint -> MID.ModuleID -> EIO (Path Abs Dir)
getModuleDirByID (MainModule pivotModule) mHint moduleID = do
  case moduleID of
    MID.Base -> do
      let message = "The base module cannot be used here"
      case mHint of
        Just hint ->
          throwError $ newError hint message
        Nothing ->
          throwError $ newError' message
    MID.Main ->
      return $ getModuleRootDir pivotModule
    MID.Library (MD.ModuleDigest digest) -> do
      dependencyDir <- Path.getDependencyDirPath pivotModule
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
  return $ filter hasSourceExtension filePathList

getAllSourceInModule :: Module -> EIO [Source.Source]
getAllSourceInModule baseModule = do
  sourcePathList <- getAllSourcePathInModule baseModule
  mapM (sourceFromPath baseModule) sourcePathList

hasSourceExtension :: Path Abs File -> Bool
hasSourceExtension path =
  case splitExtension path of
    Just (_, ext)
      | ext == sourceFileExtension ->
          True
    _ ->
      False
