module Kernel.Common.Move.Handle.Global.Artifact
  ( new,
    insert,
    lookup,
  )
where

import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Kernel.Common.Rule.Artifact qualified as A
import Kernel.Common.Rule.Handle.Global.Artifact
import Language.Common.Move.Raise (raiseCritical')
import Library.Error.Rule.EIO (EIO)
import Path
import Prelude hiding (lookup)

new :: IO Handle
new = do
  _artifactMapRef <- newIORef Map.empty
  return $ Handle {..}

insert :: Handle -> Path Abs File -> A.ArtifactTime -> IO ()
insert h path artifactTime =
  modifyIORef' (_artifactMapRef h) $ Map.insert path artifactTime

lookup :: Handle -> Path Abs File -> EIO A.ArtifactTime
lookup h path = do
  amap <- liftIO $ readIORef (_artifactMapRef h)
  case Map.lookup path amap of
    Just artifactTime ->
      return artifactTime
    Nothing ->
      raiseCritical' $ "No artifact time is registered for the source: " <> T.pack (toFilePath path)
