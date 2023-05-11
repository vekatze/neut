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
import Entity.Const
import Entity.DataSize qualified as DS
import Entity.Hint
import Entity.OS qualified as OS
import Entity.Source qualified as Source
import Entity.TargetPlatform
import Path
import System.Environment
import System.Info qualified as SI

getTargetPlatform :: App TargetPlatform
getTargetPlatform =
  readRef "targetPlatform" targetPlatform

setTargetPlatform :: App ()
setTargetPlatform = do
  mTargetArch <- liftIO $ lookupEnv envVarTargetArch
  mTargetOS <- liftIO $ lookupEnv envVarTargetOS
  let targetOS = T.pack $ fromMaybe SI.os mTargetOS
  let targetArch = T.pack $ fromMaybe SI.arch mTargetArch
  writeRef targetPlatform $ TargetPlatform {os = OS.reflect targetOS, arch = Arch.reflect targetArch}

setCurrentSource :: Source.Source -> App ()
setCurrentSource =
  writeRef currentSource

getCurrentSource :: App Source.Source
getCurrentSource =
  readRef "currentSource" currentSource

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
  tp <- getTargetPlatform
  let mDataSize = Arch.dataSizeOf (arch tp)
  case mDataSize of
    Just dataSize ->
      return dataSize
    Nothing -> do
      let message = "the data size of the target platform `" <> reify tp <> "` is unknown"
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
