module Kernel.Move.Context.Global.Artifact
  ( Handle,
    new,
    insert,
    lookup,
  )
where

import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Error.Rule.EIO (EIO)
import Kernel.Common.Rule.Artifact qualified as A
import Language.Common.Move.Raise (raiseCritical')
import Path
import Prelude hiding (lookup)

newtype Handle = Handle
  { artifactMapRef :: IORef (Map.HashMap (Path Abs File) A.ArtifactTime)
  }

new :: IO Handle
new = do
  artifactMapRef <- newIORef Map.empty
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
