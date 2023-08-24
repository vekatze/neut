module Context.Env where

import Context.App
import Context.App.Internal
import Context.Throw qualified as Throw
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Time
import Entity.Arch qualified as Arch
import Entity.Artifact qualified as A
import Entity.BuildMode qualified as BM
import Entity.Const
import Entity.DataSize qualified as DS
import Entity.Hint
import Entity.LocationTree qualified as LT
import Entity.OS qualified as OS
import Entity.Platform
import Entity.Source qualified as Source
import Path
import System.Environment
import System.Info qualified as SI

getPlatform :: App Platform
getPlatform =
  readRef "platform" platform

setPlatform :: App ()
setPlatform = do
  mHostArch <- liftIO $ lookupEnv envVarTargetArch
  mHostOS <- liftIO $ lookupEnv envVarTargetOS
  let hostOS = T.pack $ fromMaybe SI.os mHostOS
  let hostArch = T.pack $ fromMaybe SI.arch mHostArch
  writeRef platform $ Platform {os = OS.reflect hostOS, arch = Arch.reflect hostArch}

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
      Throw.raiseCritical' $ "no artifact time is registered for the source: " <> T.pack (toFilePath path)

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
  pl <- getPlatform
  let mDataSize = Arch.dataSizeOf (arch pl)
  case mDataSize of
    Just dataSize ->
      return dataSize
    Nothing -> do
      let message = "the data size of the platform `" <> reify pl <> "` is unknown"
      case mm of
        Just m ->
          Throw.raiseError m message
        Nothing ->
          Throw.raiseError' message

getMainType :: App T.Text
getMainType = do
  dataSize <- getDataSize'
  case dataSize of
    DS.DataSize64 ->
      return "i64"

getBaseSize :: Hint -> App Int
getBaseSize m = do
  DS.reify <$> getDataSize m

getBaseSize' :: App Int
getBaseSize' = do
  DS.reify <$> getDataSize'
