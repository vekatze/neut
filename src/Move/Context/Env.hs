module Move.Context.Env
  ( Handle,
    new,
    PathMap,
    getArch,
    getBaseSize,
    getBaseSize',
    getBuildMode,
    getDataSize,
    getDataSize',
    getDataSize'',
    getMainModule,
    getOS,
    getPlatform,
    setBuildMode,
    setMainModule,
    setSilentMode,
    getSilentMode,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Data.Time
import Move.Context.EIO (EIO, raiseCritical', raiseError, raiseError')
import Path
import Rule.Arch qualified as Arch
import Rule.BuildMode qualified as BM
import Rule.DataSize qualified as DS
import Rule.Hint
import Rule.Module
import Rule.OS qualified as O
import Rule.Platform
import System.Info qualified as SI

data Handle
  = Handle
  { buildModeRef :: IORef BM.BuildMode,
    enableSilentModeRef :: IORef Bool,
    mainModuleRef :: IORef (Maybe MainModule)
  }

new :: IO Handle
new = do
  buildModeRef <- newIORef BM.Develop
  enableSilentModeRef <- newIORef False
  mainModuleRef <- newIORef Nothing
  return $ Handle {..}

getMainModule :: Handle -> EIO MainModule
getMainModule h =
  readIORefOrFail "mainModule" (mainModuleRef h)

setMainModule :: Handle -> MainModule -> IO ()
setMainModule h m =
  writeIORef (mainModuleRef h) (Just m)

setBuildMode :: Handle -> BM.BuildMode -> IO ()
setBuildMode h =
  writeIORef (buildModeRef h)

getBuildMode :: Handle -> IO BM.BuildMode
getBuildMode h =
  readIORef (buildModeRef h)

type PathMap = Map.HashMap (Path Abs File) UTCTime

getDataSize :: Hint -> EIO DS.DataSize
getDataSize m = do
  getDataSize'' (Just m)

getDataSize' :: EIO DS.DataSize
getDataSize' = do
  getDataSize'' Nothing

getDataSize'' :: Maybe Hint -> EIO DS.DataSize
getDataSize'' mm = do
  Arch.dataSizeOf <$> getArch mm

getBaseSize :: Hint -> EIO Int
getBaseSize m = do
  DS.reify <$> getDataSize m

getBaseSize' :: EIO Int
getBaseSize' = do
  DS.reify <$> getDataSize'

getArch :: Maybe Hint -> EIO Arch.Arch
getArch mm = do
  case SI.arch of
    "amd64" ->
      return Arch.Amd64
    "x86_64" ->
      return Arch.Amd64
    "arm64" ->
      return Arch.Arm64
    "aarch64" ->
      return Arch.Arm64
    arch ->
      case mm of
        Just m ->
          raiseError m $ "Unknown architecture: " <> T.pack arch
        Nothing ->
          raiseError' $ "Unknown architecture: " <> T.pack arch

getOS :: Maybe Hint -> EIO O.OS
getOS mm = do
  case SI.os of
    "linux" ->
      return O.Linux
    "darwin" ->
      return O.Darwin
    os ->
      case mm of
        Just m ->
          raiseError m $ "Unknown OS: " <> T.pack os
        Nothing ->
          raiseError' $ "Unknown OS: " <> T.pack os

getPlatform :: Maybe Hint -> EIO Platform
getPlatform mm = do
  arch <- getArch mm
  os <- getOS mm
  return $ Platform {arch, os}

setSilentMode :: Handle -> Bool -> IO ()
setSilentMode h =
  writeIORef (enableSilentModeRef h)

getSilentMode :: Handle -> IO Bool
getSilentMode h =
  readIORef (enableSilentModeRef h)

readIORefOrFail :: T.Text -> IORef (Maybe a) -> EIO a
readIORefOrFail name ref = do
  mValue <- liftIO $ readIORef ref
  case mValue of
    Just a ->
      return a
    Nothing ->
      raiseCritical' $ "[compiler bug] `" <> name <> "` is uninitialized"
