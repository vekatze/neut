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
import Data.Set qualified as S
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Rule.Hint
import Rule.Ident
import Rule.Ident.Reify
import Rule.VarDefKind
import Prelude hiding (lookup, read)

data Handle
  = Handle
  { unusedVariableMapRef :: IORef (IntMap.IntMap (Hint, Ident, VarDefKind)),
    usedVariableSetRef :: IORef (S.Set Int)
  }

new :: App Handle
new = do
  unusedVariableMapRef <- asks App.unusedVariableMap
  usedVariableSetRef <- asks App.usedVariableSet
  return $ Handle {..}

initialize :: App ()
initialize = do
  writeRef' App.unusedVariableMap IntMap.empty
  writeRef' App.usedVariableSet S.empty

insert :: Handle -> Hint -> Ident -> VarDefKind -> IO ()
insert h m x k =
  modifyIORef' (unusedVariableMapRef h) $ IntMap.insert (toInt x) (m, x, k)

delete :: Handle -> Ident -> IO ()
delete h x =
  modifyIORef' (usedVariableSetRef h) $ S.insert (toInt x)

get :: Handle -> IO [(Hint, Ident, VarDefKind)]
get h = do
  vars <- readIORef (unusedVariableMapRef h)
  usedVarSet <- readIORef (usedVariableSetRef h)
  return $ filter (\(_, var, _) -> not (isHole var) && S.notMember (toInt var) usedVarSet) $ IntMap.elems vars
