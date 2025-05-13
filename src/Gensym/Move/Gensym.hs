module Gensym.Move.Gensym (newCount) where

import Gensym.Rule.Handle
import Data.IORef

{-# INLINE newCount #-}
newCount :: Handle -> IO Int
newCount h =
  atomicModifyIORef' (_counterRef h) (\x -> (x + 1, x))
