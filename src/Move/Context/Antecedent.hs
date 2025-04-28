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
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Set qualified as S
import Data.Text qualified as T
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

new :: IO Handle
new = do
  antecedentMapRef <- newIORef Map.empty
  reverseAntecedentMapRef <- newIORef Map.empty
  antecedentDigestCacheRef <- newIORef Nothing
  return $ Handle {..}

initialize :: Handle -> IO ()
initialize h = do
  writeIORef (antecedentMapRef h) Map.empty
  writeIORef (reverseAntecedentMapRef h) Map.empty
  writeIORef (antecedentDigestCacheRef h) Nothing

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
