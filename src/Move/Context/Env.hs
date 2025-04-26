module Move.Context.Env
  ( Handle,
    new,
    PathMap,
    getArch,
    getBaseSize,
    getBaseSize',
    getBuildMode,
    getCurrentSource,
    getCurrentSource',
    getDataSize,
    getDataSize',
    getDataSize'',
    getMainModule,
    getOS,
    getPlatform,
    setBuildMode,
    setCurrentSource,
    setMainModule,
    setSilentMode,
    getSilentMode,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Data.Time
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.EIO (EIO, raiseCritical', raiseError, raiseError')
import Path
import Rule.Arch qualified as Arch
import Rule.BuildMode qualified as BM
import Rule.DataSize qualified as DS
import Rule.Hint
import Rule.Module
import Rule.OS qualified as O
import Rule.Platform
import Rule.Source qualified as Source
import System.Info qualified as SI

newtype Handle
  = Handle
  { currentSourceRef :: IORef (Maybe Source.Source)
  }

new :: App Handle
new = do
  currentSourceRef <- asks App.currentSource
  return $ Handle {..}

getMainModule :: App MainModule
getMainModule =
  readRef "mainModule" App.mainModule

setMainModule :: MainModule -> App ()
setMainModule =
  writeRef App.mainModule

setBuildMode :: BM.BuildMode -> App ()
setBuildMode =
  writeRef' App.buildMode

getBuildMode :: App BM.BuildMode
getBuildMode =
  readRef' App.buildMode

setCurrentSource :: Handle -> Source.Source -> IO ()
setCurrentSource h s =
  writeIORef (currentSourceRef h) (Just s)

getCurrentSource :: Handle -> IO Source.Source
getCurrentSource h = do
  mValue <- readIORef $ currentSourceRef h
  case mValue of
    Just a ->
      return a
    Nothing ->
      error $ T.unpack "[compiler bug] `currentSource` is uninitialized"

getCurrentSource' :: IORef (Maybe Source.Source) -> EIO Source.Source
getCurrentSource' ref = do
  sourceOrNone <- liftIO $ readIORef ref
  case sourceOrNone of
    Nothing ->
      raiseCritical' "[compiler bug] `currentSource` is uninitialized"
    Just source ->
      return source

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

setSilentMode :: Bool -> App ()
setSilentMode =
  writeRef' App.enableSilentMode

getSilentMode :: App Bool
getSilentMode =
  readRef' App.enableSilentMode
