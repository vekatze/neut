module Language.Comp.Move.CreateVar (createVar) where

import Aux.Gensym.Rule.Handle
import Data.Text qualified as T
import Language.Common.Move.CreateSymbol
import Language.Common.Rule.Ident
import Language.Comp.Rule.Comp qualified as C

{-# INLINE createVar #-}
createVar :: Handle -> T.Text -> IO (Ident, C.Value)
createVar h name = do
  x <- newIdentFromText h name
  return (x, C.VarLocal x)
