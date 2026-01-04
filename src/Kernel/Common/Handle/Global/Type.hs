module Kernel.Common.Handle.Global.Type
  ( Handle (..),
    new,
    insert',
    lookup',
    lookupMaybe',
  )
where

import App.App (App)
import App.Run (raiseCritical)
import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Language.Common.DefiniteDescription qualified as DD
import Language.WeakTerm.WeakTerm
import Logger.Hint
import Prelude hiding (lookup)

newtype Handle = Handle
  { _typeEnvRef :: IORef (Map.HashMap DD.DefiniteDescription WeakType)
  }

new :: IO Handle
new = do
  _typeEnvRef <- newIORef Map.empty
  return $ Handle {..}

insert' :: Handle -> DD.DefiniteDescription -> WeakType -> IO ()
insert' h k v = do
  putStrLn $ "insert: " <> T.unpack (DD.reify k)
  modifyIORef' (_typeEnvRef h) $ Map.insert k v

lookup' :: Handle -> Hint -> DD.DefiniteDescription -> App WeakType
lookup' h m k = do
  valueOrNone <- liftIO $ lookupMaybe' h k
  case valueOrNone of
    Just value ->
      return value
    Nothing -> do
      typeEnv <- liftIO $ readIORef (_typeEnvRef h)
      let ks = map DD.reify $ Map.keys typeEnv
      raiseCritical m $ "`" <> DD.reify k <> "` is not found in the term type environment.\nkeys:" <> T.pack (show ks)

lookupMaybe' :: Handle -> DD.DefiniteDescription -> IO (Maybe WeakType)
lookupMaybe' h k = do
  typeEnv <- readIORef (_typeEnvRef h)
  return $ Map.lookup k typeEnv
