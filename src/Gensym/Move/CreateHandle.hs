module Gensym.Move.CreateHandle (createHandle) where

import Data.IORef
import Gensym.Rule.Handle

createHandle :: IO Handle
createHandle = do
  _counterRef <- newIORef 0
  return $ InternalHandle {..}
