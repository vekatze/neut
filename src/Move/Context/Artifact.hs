module Move.Context.Artifact
  ( Handle,
    new,
    insert,
    lookup,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Move.Context.App (App)
import Move.Context.App.Internal qualified as App
import Move.Context.EIO (EIO, raiseCritical')
import Path
import Rule.Artifact qualified as A
import Prelude hiding (lookup)

newtype Handle
  = Handle
  { artifactMapRef :: IORef (Map.HashMap (Path Abs File) A.ArtifactTime)
  }

new :: App Handle
new = do
  artifactMapRef <- asks App.artifactMap
  return $ Handle {..}

insert :: Handle -> Path Abs File -> A.ArtifactTime -> IO ()
insert h path artifactTime =
  modifyIORef' (artifactMapRef h) $ Map.insert path artifactTime

lookup :: Handle -> Path Abs File -> EIO A.ArtifactTime
lookup h path = do
  amap <- liftIO $ readIORef (artifactMapRef h)
  case Map.lookup path amap of
    Just artifactTime ->
      return artifactTime
    Nothing ->
      raiseCritical' $ "No artifact time is registered for the source: " <> T.pack (toFilePath path)
