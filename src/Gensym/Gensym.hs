module Gensym.Gensym (newCount) where

import Data.IORef
import Gensym.Handle

{-# INLINE newCount #-}
newCount :: Handle -> IO Int
newCount h =
  atomicModifyIORef' (_counterRef h) (\x -> (x + 1, x))
