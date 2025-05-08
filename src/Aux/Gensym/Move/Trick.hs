module Aux.Gensym.Move.Trick (setCount, getCount) where

import Aux.Gensym.Rule.Handle
import Data.IORef

setCount :: Handle -> Int -> IO ()
setCount h countSnapshot = do
  atomicModifyIORef' (_counterRef h) (\x -> (max x countSnapshot, ()))

getCount :: Handle -> IO Int
getCount h =
  readIORef (_counterRef h)
