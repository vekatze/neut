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
import Language.Common.DefiniteDescription qualified as DD
import Language.WeakTerm.WeakTerm
import Logger.Hint
import Prelude hiding (lookup)

newtype Handle = Handle
  { _typeEnvRef :: IORef (Map.HashMap DD.DefiniteDescription WeakTerm)
  }

new :: IO Handle
new = do
  _typeEnvRef <- newIORef Map.empty
  return $ Handle {..}

insert' :: Handle -> DD.DefiniteDescription -> WeakTerm -> IO ()
insert' h k v =
  modifyIORef' (_typeEnvRef h) $ Map.insert k v

lookup' :: Handle -> Hint -> DD.DefiniteDescription -> App WeakTerm
lookup' h m k = do
  valueOrNone <- liftIO $ lookupMaybe' h k
  case valueOrNone of
    Just value ->
      return value
    Nothing ->
      raiseCritical m $ "`" <> DD.reify k <> "` is not found in the term type environment."

lookupMaybe' :: Handle -> DD.DefiniteDescription -> IO (Maybe WeakTerm)
lookupMaybe' h k = do
  typeEnv <- readIORef (_typeEnvRef h)
  return $ Map.lookup k typeEnv
