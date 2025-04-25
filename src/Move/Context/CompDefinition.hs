module Move.Context.CompDefinition
  ( Handle,
    new,
    DefKey,
    DefValue,
    DefMap,
    insert,
    lookup,
  )
where

import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Rule.Comp
import Rule.DefiniteDescription qualified as DD
import Rule.Ident
import Rule.Opacity
import Rule.Opacity qualified as O
import Prelude hiding (lookup, read)

type DefKey = DD.DefiniteDescription

type DefValue = (Opacity, [Ident], Comp)

type DefMap = Map.HashMap DD.DefiniteDescription (Opacity, [Ident], Comp)

newtype Handle
  = Handle
  { compEnvRef :: IORef (Map.HashMap DD.DefiniteDescription (O.Opacity, [Ident], Comp))
  }

new :: App Handle
new = do
  compEnvRef <- asks App.compEnv
  return $ Handle {..}

insert :: Handle -> DefKey -> DefValue -> IO ()
insert h k v =
  modifyIORef' (compEnvRef h) $ Map.insert k v

lookup :: Handle -> DefKey -> IO (Maybe DefValue)
lookup h k = do
  cenv <- readIORef (compEnvRef h)
  return $ Map.lookup k cenv
