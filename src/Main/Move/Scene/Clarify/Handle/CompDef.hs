module Main.Move.Scene.Clarify.Handle.CompDef
  ( Handle,
    new,
    DefKey,
    DefValue,
    insert,
    get,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Ident
import Language.Common.Rule.Opacity
import Language.Common.Rule.Opacity qualified as O
import Language.Comp.Rule.Comp
import Prelude hiding (lookup, read)

type DefKey = DD.DefiniteDescription

type DefValue = (Opacity, [Ident], Comp)

newtype Handle = Handle
  { compEnvRef :: IORef (Map.HashMap DD.DefiniteDescription (O.Opacity, [Ident], Comp))
  }

new :: IO Handle
new = do
  compEnvRef <- newIORef Map.empty
  return $ Handle {..}

insert :: Handle -> DefKey -> DefValue -> IO ()
insert h k v =
  modifyIORef' (compEnvRef h) $ Map.insert k v

get :: Handle -> IO DefMap
get h = do
  readIORef (compEnvRef h)
