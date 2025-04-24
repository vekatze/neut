module Move.Context.Definition
  ( Handle,
    new,
    initialize,
    insert,
    get,
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

insert :: O.Opacity -> DD.DefiniteDescription -> [BinderF TM.Term] -> TM.Term -> App ()
insert opacity name xts e =
  when (opacity == O.Clear) $
    modifyRef' App.defMap $
      Map.insert name (xts, e)

get :: App (Map.HashMap DD.DefiniteDescription ([BinderF TM.Term], TM.Term))
get =
  readRef' App.defMap

insert' :: Handle -> O.Opacity -> DD.DefiniteDescription -> [BinderF TM.Term] -> TM.Term -> IO ()
insert' h opacity name xts e =
  when (opacity == O.Clear) $
    modifyIORef' (defMapRef h) $
      Map.insert name (xts, e)

get' :: Handle -> IO (Map.HashMap DD.DefiniteDescription ([BinderF TM.Term], TM.Term))
get' h =
  readIORef (defMapRef h)
