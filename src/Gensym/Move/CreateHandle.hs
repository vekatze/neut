module Gensym.Move.CreateHandle (createHandle) where

import Gensym.Rule.Handle
import Data.IORef

createHandle :: IO Handle
createHandle = do
  _counterRef <- newIORef 0
  return $ InternalHandle {..}
