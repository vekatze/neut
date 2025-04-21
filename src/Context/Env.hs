module Context.Env
  ( PathMap,
    getArch,
    getArtifactMap,
    getBaseSize,
    getBaseSize',
    getBuildMode,
    getCurrentSource,
    getDataSize,
    getDataSize',
    getDataSize'',
    getMainModule,
    getOS,
    getPlatform,
    getTagMap,
    insertToArtifactMap,
    lookupArtifactTime,
    setBuildMode,
    setCurrentSource,
    setMainModule,
    setSilentMode,
    getSilentMode,
  )
where

import Context.App
import Context.App.Internal
import Context.Throw qualified as Throw
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Data.Time
import Rule.Arch qualified as Arch
import Rule.Artifact qualified as A
import Rule.BuildMode qualified as BM
import Rule.DataSize qualified as DS
import Rule.Hint
import Rule.LocationTree qualified as LT
import Rule.Module
import Rule.OS qualified as O
import Rule.Platform
import Rule.Source qualified as Source
import Path
import System.Info qualified as SI

getMainModule :: App Module
getMainModule =
  readRef "mainModule" mainModule

setMainModule :: Module -> App ()
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

getTagMap :: App LT.LocationTree
getTagMap =
  readRef' tagMap

type PathMap = Map.HashMap (Path Abs File) UTCTime

lookupArtifactTime :: Path Abs File -> App A.ArtifactTime
lookupArtifactTime path = do
  amap <- readRef' artifactMap
  case Map.lookup path amap of
    Just artifactTime ->
      return artifactTime
    Nothing ->
      Throw.raiseCritical' $ "No artifact time is registered for the source: " <> T.pack (toFilePath path)

getArtifactMap :: App (Map.HashMap (Path Abs File) A.ArtifactTime)
getArtifactMap =
  readRef' artifactMap

insertToArtifactMap :: Path Abs File -> A.ArtifactTime -> App ()
insertToArtifactMap path artifactTime =
  modifyRef' artifactMap $ Map.insert path artifactTime

getDataSize :: Hint -> App DS.DataSize
getDataSize m = do
  getDataSize'' (Just m)

getDataSize' :: App DS.DataSize
getDataSize' = do
  getDataSize'' Nothing

getDataSize'' :: Maybe Hint -> App DS.DataSize
getDataSize'' mm = do
  Arch.dataSizeOf <$> getArch mm

getBaseSize :: Hint -> App Int
getBaseSize m = do
  DS.reify <$> getDataSize m

getBaseSize' :: App Int
getBaseSize' = do
  DS.reify <$> getDataSize'

getArch :: Maybe Hint -> App Arch.Arch
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
          Throw.raiseError m $ "Unknown architecture: " <> T.pack arch
        Nothing ->
          Throw.raiseError' $ "Unknown architecture: " <> T.pack arch

getOS :: Maybe Hint -> App O.OS
getOS mm = do
  case SI.os of
    "linux" ->
      return O.Linux
    "darwin" ->
      return O.Darwin
    os ->
      case mm of
        Just m ->
          Throw.raiseError m $ "Unknown OS: " <> T.pack os
        Nothing ->
          Throw.raiseError' $ "Unknown OS: " <> T.pack os

getPlatform :: Maybe Hint -> App Platform
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
