module Context.Env where

import Context.App
import Context.App.Internal
import Context.Throw qualified as Throw
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Data.Time
import Entity.Arch qualified as Arch
import Entity.Artifact qualified as A
import Entity.BuildMode qualified as BM
import Entity.DataSize qualified as DS
import Entity.DefiniteDescription (getLocalName)
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.Hint.Reify qualified as H
import Entity.LocationTree qualified as LT
import Entity.Macro qualified as Macro
import Entity.Platform
import Entity.Source qualified as Source
import Entity.Tree
import Path

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
  let mDataSize = Arch.dataSizeOf (arch platform)
  case mDataSize of
    Just dataSize ->
      return dataSize
    Nothing -> do
      let message = "the data size of the platform `" <> reify platform <> "` is unknown"
      case mm of
        Just m ->
          Throw.raiseError m message
        Nothing ->
          Throw.raiseError' message

clearMacroEnv :: App ()
clearMacroEnv = do
  writeRef' macroRuleEnv Map.empty

getMacroEnv :: App Macro.Rules
getMacroEnv =
  readRef' macroRuleEnv

insertToMacroEnv :: Hint -> DD.DefiniteDescription -> [(Macro.Args, Tree)] -> App ()
insertToMacroEnv m key value = do
  menv <- readRef' macroRuleEnv
  let localName = getLocalName key
  case Map.lookup localName menv of
    Just (mExisting, _) -> do
      Throw.raiseError m $ "The macro " <> localName <> "is already defined at:\n" <> T.pack (H.toString mExisting)
    Nothing -> do
      modifyRef' macroRuleEnv $ Map.insert localName (m, value)

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
