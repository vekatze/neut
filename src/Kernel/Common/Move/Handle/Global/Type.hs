module Kernel.Common.Move.Handle.Global.Type
  ( new,
    insert',
    lookup',
    lookupMaybe',
  )
where

import Control.Monad.IO.Class
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Kernel.Common.Rule.Handle.Global.Type
import Language.Common.Move.Raise (raiseCritical)
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.WeakTerm.Rule.WeakTerm
import Library.Error.Rule.EIO (EIO)
import Library.Logger.Rule.Hint
import Prelude hiding (lookup)

new :: IO Handle
new = do
  _typeEnvRef <- newIORef Map.empty
  return $ Handle {..}

insert' :: Handle -> DD.DefiniteDescription -> WeakTerm -> IO ()
insert' h k v =
  modifyIORef' (_typeEnvRef h) $ Map.insert k v

lookup' :: Handle -> Hint -> DD.DefiniteDescription -> EIO WeakTerm
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
