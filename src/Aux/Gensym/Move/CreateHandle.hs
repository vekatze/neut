module Aux.Gensym.Move.CreateHandle (createHandle) where

import Aux.Gensym.Rule.Handle
import Data.IORef

createHandle :: IO Handle
createHandle = do
  _counterRef <- newIORef 0
  return $ InternalHandle {..}
