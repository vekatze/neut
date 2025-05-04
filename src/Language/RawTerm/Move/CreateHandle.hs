module Language.RawTerm.Move.CreateHandle (createHandle) where

import Gensym.Rule.Handle qualified as Gensym
import Language.RawTerm.Rule.Handle

createHandle :: Gensym.Handle -> IO Handle
createHandle _gensymHandle = do
  return $ InternalHandle {..}
