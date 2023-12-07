module Context.Module
  ( getModuleFilePath,
    getModuleDirByID,
    getMainModule,
    setMainModule,
    getModuleCacheMap,
    getCoreModuleURL,
    getCoreModuleDigest,
    insertToModuleCacheMap,
    saveEns,
  )
where

import Context.App
import Context.App.Internal
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.Const
import Entity.Ens
import Entity.Ens.Reify qualified as Ens
import Entity.Hint qualified as H
import Entity.Module
import Entity.ModuleDigest
import Entity.ModuleDigest qualified as MD
import Entity.ModuleID qualified as MID
import Entity.ModuleURL
import Path
import Path.IO
import System.Environment

getMainModule :: App Module
getMainModule =
  readRef "mainModule" mainModule

setMainModule :: Module -> App ()
setMainModule =
  writeRef mainModule

getModuleFilePath :: Maybe H.Hint -> MID.ModuleID -> App (Path Abs File)
getModuleFilePath mHint moduleID = do
  moduleDir <- getModuleDirByID mHint moduleID
  return $ moduleDir </> moduleFile

getModuleCacheMap :: App (Map.HashMap (Path Abs File) Module)
getModuleCacheMap =
  readRef' moduleCacheMap

insertToModuleCacheMap :: Path Abs File -> Module -> App ()
insertToModuleCacheMap k v =
  modifyRef' moduleCacheMap $ Map.insert k v

getModuleDirByID :: Maybe H.Hint -> MID.ModuleID -> App (Path Abs Dir)
getModuleDirByID mHint moduleID = do
  mainModule <- getMainModule
  case moduleID of
    MID.Base -> do
      let message = "the base module can't be used here"
      case mHint of
        Just hint ->
          Throw.raiseError hint message
        Nothing ->
          Throw.raiseError' message
    MID.Main ->
      return $ getModuleRootDir mainModule
    MID.Library (MD.ModuleDigest digest) -> do
      libraryDir <- Path.getLibraryDirPath
      resolveDir libraryDir $ T.unpack digest

saveEns :: Path Abs File -> FullEns -> App ()
saveEns path (c1, (ens, c2)) = do
  ens' <- Throw.liftEither $ stylize ens
  Path.writeText path $ Ens.pp (c1, (ens', c2))

getCoreModuleURL :: App ModuleURL
getCoreModuleURL = do
  mCoreModuleURL <- liftIO $ lookupEnv envVarCoreModuleURL
  case mCoreModuleURL of
    Just coreModuleURL ->
      return $ ModuleURL $ T.pack coreModuleURL
    Nothing ->
      Throw.raiseError' $ "the URL of the core module isn't specified; set it via " <> T.pack envVarCoreModuleURL

getCoreModuleDigest :: App ModuleDigest
getCoreModuleDigest = do
  mCoreModuleDigest <- liftIO $ lookupEnv envVarCoreModuleDigest
  case mCoreModuleDigest of
    Just coreModuleDigest ->
      return $ ModuleDigest $ T.pack coreModuleDigest
    Nothing ->
      Throw.raiseError' $ "the digest of the core module isn't specified; set it via " <> T.pack envVarCoreModuleDigest
