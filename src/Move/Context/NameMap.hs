module Move.Context.NameMap
  ( Handle,
    new,
    saveCurrentNameSet,
    lookupSourceNameMap,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Move.Context.EIO (EIO, raiseCritical)
import Path
import Rule.Hint qualified as Hint
import Rule.TopNameMap
import Prelude hiding (lookup)

newtype Handle
  = Handle
  { sourceNameMapRef :: IORef (Map.HashMap (Path Abs File) TopNameMap)
  }

new :: IO Handle
new = do
  sourceNameMapRef <- newIORef Map.empty
  return $ Handle {..}

lookupSourceNameMap :: Handle -> Hint.Hint -> Path Abs File -> EIO TopNameMap
lookupSourceNameMap h m sourcePath = do
  smap <- liftIO $ readIORef (sourceNameMapRef h)
  case Map.lookup sourcePath smap of
    Just topLevelNameInfo -> do
      return topLevelNameInfo
    Nothing ->
      raiseCritical m $ "Top-level names for " <> T.pack (toFilePath sourcePath) <> " is not registered"

saveCurrentNameSet :: Handle -> Path Abs File -> TopNameMap -> IO ()
saveCurrentNameSet h currentPath nameMap = do
  modifyIORef' (sourceNameMapRef h) $ Map.insert currentPath nameMap
