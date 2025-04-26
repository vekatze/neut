module Move.Context.Antecedent
  ( RevMap,
    Handle,
    new,
    initialize,
    get,
    set,
    getReverseMap,
  )
where

import Control.Monad
import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Set qualified as S
import Data.Text qualified as T
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Rule.Module qualified as M
import Rule.ModuleID qualified as MID
import Prelude hiding (lookup, read)

data Handle
  = Handle
  { antecedentMapRef :: IORef (Map.HashMap MID.ModuleID M.Module),
    reverseAntecedentMapRef :: IORef (Map.HashMap MID.ModuleID (S.Set MID.ModuleID)),
    antecedentDigestCacheRef :: IORef (Maybe T.Text)
  }

type RevMap =
  Map.HashMap MID.ModuleID (S.Set MID.ModuleID)

new :: App Handle
new = do
  antecedentMapRef <- asks App.antecedentMap
  reverseAntecedentMapRef <- asks App.reverseAntecedentMap
  antecedentDigestCacheRef <- asks App.antecedentDigestCache
  return $ Handle {..}

initialize :: App ()
initialize = do
  writeRef' App.antecedentMap Map.empty
  writeRef' App.reverseAntecedentMap Map.empty
  writeRef' App.antecedentDigestCache Nothing

get :: Handle -> IO (Map.HashMap MID.ModuleID M.Module)
get h =
  readIORef (antecedentMapRef h)

set :: Handle -> Map.HashMap MID.ModuleID M.Module -> IO ()
set h mp = do
  writeIORef (antecedentMapRef h) mp
  forM_ (Map.toList mp) $ \(mid, m) -> do
    modifyIORef' (reverseAntecedentMapRef h) $ Map.insertWith S.union (M.moduleID m) (S.singleton mid)

getReverseMap :: Handle -> IO RevMap
getReverseMap h =
  readIORef (reverseAntecedentMapRef h)
