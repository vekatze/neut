module Kernel.Common.HashMapRef (insertEntries) where

import Control.Monad (unless)
import Data.HashMap.Strict qualified as Map
import Data.Hashable (Hashable)
import Data.IORef

insertEntries :: (Hashable k) => IORef (Map.HashMap k v) -> [(k, v)] -> IO ()
insertEntries ref entries =
  unless (null entries) $
    atomicModifyIORef' ref $ \mp ->
      (foldl' (\acc (k, v) -> Map.insert k v acc) mp entries, ())
