module Move.Context.Env
  ( PathMap,
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
    lookupArtifactTime,
    setBuildMode,
    setCurrentSource,
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
import Move.Context.App
import Move.Context.App.Internal
import Move.Context.EIO (EIO, raiseCritical', raiseError, raiseError')
import Path
import Rule.Arch qualified as Arch
import Rule.Artifact qualified as A
import Rule.BuildMode qualified as BM
import Rule.DataSize qualified as DS
import Rule.Hint
import Rule.Module
import Rule.OS qualified as O
import Rule.Platform
import Rule.Source qualified as Source
import System.Info qualified as SI

getMainModule :: App MainModule
getMainModule =
  readRef "mainModule" mainModule

setMainModule :: MainModule -> App ()
setMainModule =
  writeRef mainModule

setBuildMode :: BM.BuildMode -> App ()
setBuildMode =
  writeRef' buildMode

getBuildMode :: App BM.BuildMode
getBuildMode =
  readRef' buildMode

setCurrentSource :: Source.Source -> App ()
setCurrentSource =
  writeRef currentSource

getCurrentSource :: App Source.Source
getCurrentSource =
  readRef "currentSource" currentSource

getCurrentSource' :: IORef (Maybe Source.Source) -> EIO Source.Source
getCurrentSource' ref = do
  sourceOrNone <- liftIO $ readIORef ref
  case sourceOrNone of
    Nothing ->
      raiseCritical' "[compiler bug] `currentSource` is uninitialized"
    Just source ->
      return source

type PathMap = Map.HashMap (Path Abs File) UTCTime

type ArtifactTimeRef =
  IORef (Map.HashMap (Path Abs File) A.ArtifactTime)

lookupArtifactTime :: ArtifactTimeRef -> Path Abs File -> EIO A.ArtifactTime
lookupArtifactTime ref path = do
  amap <- liftIO $ readIORef ref
  case Map.lookup path amap of
    Just artifactTime ->
      return artifactTime
    Nothing ->
      raiseCritical' $ "No artifact time is registered for the source: " <> T.pack (toFilePath path)

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
  writeRef' enableSilentMode

getSilentMode :: App Bool
getSilentMode =
  readRef' enableSilentMode
