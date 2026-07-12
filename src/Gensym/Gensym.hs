module Gensym.Gensym (newCount) where

import Data.IORef
import Gensym.Handle

{-# INLINE newCount #-}
newCount :: Handle -> IO Int
newCount h = do
  count <- readIORef (_counterRef h)
  writeIORef (_counterRef h) $! count + 1
  return count
