module Move.Context.Definition
  ( Handle,
    new,
    DefMap,
    initialize,
    insert',
    get',
  )
where

import Control.Monad
import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Rule.Binder
import Rule.DefiniteDescription qualified as DD
import Rule.Opacity qualified as O
import Rule.Term qualified as TM
import Prelude hiding (lookup, read)

type DefMap =
  Map.HashMap DD.DefiniteDescription ([BinderF TM.Term], TM.Term)

newtype Handle
  = Handle
  { defMapRef :: IORef (Map.HashMap DD.DefiniteDescription ([BinderF TM.Term], TM.Term))
  }

new :: App Handle
new = do
  defMapRef <- asks App.defMap
  return $ Handle {..}

initialize :: App ()
initialize = do
  writeRef' App.defMap Map.empty

insert' :: Handle -> O.Opacity -> DD.DefiniteDescription -> [BinderF TM.Term] -> TM.Term -> IO ()
insert' h opacity name xts e =
  when (opacity == O.Clear) $
    modifyIORef' (defMapRef h) $
      Map.insert name (xts, e)

get' :: Handle -> IO (Map.HashMap DD.DefiniteDescription ([BinderF TM.Term], TM.Term))
get' h =
  readIORef (defMapRef h)
