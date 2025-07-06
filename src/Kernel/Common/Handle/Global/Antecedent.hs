module Kernel.Common.Handle.Global.Antecedent
  ( Handle,
    RevMap,
    new,
    get,
    set,
    getReverseMap,
  )
where

import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Set qualified as S
import Kernel.Common.Module qualified as M
import Kernel.Common.RuleHandle.Global.Antecedent (Handle (..), RevMap)
import Language.Common.ModuleID qualified as MID
import Prelude hiding (lookup, read)

new :: IO Handle
new = do
  _antecedentMapRef <- newIORef Map.empty
  _reverseAntecedentMapRef <- newIORef Map.empty
  return $ Handle {..}

get :: Handle -> IO (Map.HashMap MID.ModuleID M.Module)
get h =
  readIORef (_antecedentMapRef h)

set :: Handle -> Map.HashMap MID.ModuleID M.Module -> IO ()
set h mp = do
  writeIORef (_antecedentMapRef h) mp
  forM_ (Map.toList mp) $ \(mid, m) -> do
    modifyIORef' (_reverseAntecedentMapRef h) $ Map.insertWith S.union (M.moduleID m) (S.singleton mid)

getReverseMap :: Handle -> IO RevMap
getReverseMap h =
  readIORef (_reverseAntecedentMapRef h)
