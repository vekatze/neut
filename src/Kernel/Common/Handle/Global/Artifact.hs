module Kernel.Common.Handle.Global.Artifact
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
import Error.EIO (EIO)
import Error.Run (raiseCritical')
import Kernel.Common.Artifact qualified as A
import Kernel.Common.RuleHandle.Global.Artifact
import Path
import Prelude hiding (lookup)

new :: IO Handle
new = do
  _artifactMapRef <- newIORef Map.empty
  return $ Handle {..}

insert :: Handle -> Path Abs File -> A.ArtifactTime -> IO ()
insert h path artifactTime =
  atomicModifyIORef' (_artifactMapRef h) (\mp -> (Map.insert path artifactTime mp, ()))

lookup :: Handle -> Path Abs File -> EIO A.ArtifactTime
lookup h path = do
  amap <- liftIO $ readIORef (_artifactMapRef h)
  case Map.lookup path amap of
    Just artifactTime ->
      return artifactTime
    Nothing ->
      raiseCritical' $ "No artifact time is registered for the source: " <> T.pack (toFilePath path)
