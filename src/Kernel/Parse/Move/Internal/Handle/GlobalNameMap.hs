module Kernel.Parse.Move.Internal.Handle.GlobalNameMap
  ( Handle,
    new,
    insert,
    lookup,
  )
where

import Error.Move.Run (raiseCritical)
import Error.Rule.EIO (EIO)
import Logger.Rule.Hint qualified as Hint
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Kernel.Common.Rule.TopNameMap
import Path
import Prelude hiding (lookup)

newtype Handle = Handle
  { globalNameMapRef :: IORef (Map.HashMap (Path Abs File) TopNameMap)
  }

new :: IO Handle
new = do
  globalNameMapRef <- newIORef Map.empty
  return $ Handle {..}

lookup :: Handle -> Hint.Hint -> Path Abs File -> EIO TopNameMap
lookup h m sourcePath = do
  smap <- liftIO $ readIORef (globalNameMapRef h)
  case Map.lookup sourcePath smap of
    Just topLevelNameInfo -> do
      return topLevelNameInfo
    Nothing ->
      raiseCritical m $ "Top-level names for " <> T.pack (toFilePath sourcePath) <> " is not registered" <> "\n" <> T.pack (show $ Map.keys smap)

insert :: Handle -> Path Abs File -> TopNameMap -> IO ()
insert h currentPath nameMap = do
  atomicModifyIORef' (globalNameMapRef h) (\mp -> (Map.insert currentPath nameMap mp, ()))
