module Library.Gensym.Move.Trick (setCount, getCount) where

import Data.IORef
import Library.Gensym.Rule.Handle

setCount :: Handle -> Int -> IO ()
setCount h countSnapshot = do
  atomicModifyIORef' (_counterRef h) (\x -> (max x countSnapshot, ()))

getCount :: Handle -> IO Int
getCount h =
  readIORef (_counterRef h)
