module Move.Context.UnusedVariable
  ( Handle,
    new,
    initialize,
    insert,
    delete,
    get,
  )
where

import Control.Monad.Reader (asks)
import Data.IORef
import Data.IntMap qualified as IntMap
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Rule.Hint
import Rule.Ident
import Rule.Ident.Reify
import Rule.VarDefKind
import Prelude hiding (lookup, read)

newtype Handle
  = Handle
  { unusedVariableMapRef :: IORef (IntMap.IntMap (Hint, Ident, VarDefKind))
  }

new :: App Handle
new = do
  unusedVariableMapRef <- asks App.unusedVariableMap
  return $ Handle {..}

initialize :: Handle -> IO ()
initialize h = do
  writeIORef (unusedVariableMapRef h) IntMap.empty

insert :: Handle -> Hint -> Ident -> VarDefKind -> IO ()
insert h m x k =
  modifyIORef' (unusedVariableMapRef h) $ IntMap.insert (toInt x) (m, x, k)

delete :: Handle -> Ident -> IO ()
delete h x = do
  modifyIORef' (unusedVariableMapRef h) $ IntMap.delete (toInt x)

get :: Handle -> IO [(Hint, Ident, VarDefKind)]
get h = do
  vars <- readIORef (unusedVariableMapRef h)
  return $ filter (\(_, var, _) -> not (isHole var)) $ IntMap.elems vars
