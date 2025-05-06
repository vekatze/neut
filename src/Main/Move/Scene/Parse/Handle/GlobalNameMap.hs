module Main.Move.Scene.Parse.Handle.GlobalNameMap
  ( Handle,
    new,
    insert,
    lookup,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Error.Rule.EIO (EIO)
import Language.Common.Move.Raise (raiseCritical)
import Logger.Rule.Hint qualified as Hint
import Main.Rule.TopNameMap
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
      raiseCritical m $ "Top-level names for " <> T.pack (toFilePath sourcePath) <> " is not registered"

insert :: Handle -> Path Abs File -> TopNameMap -> IO ()
insert h currentPath nameMap = do
  modifyIORef' (globalNameMapRef h) $ Map.insert currentPath nameMap
