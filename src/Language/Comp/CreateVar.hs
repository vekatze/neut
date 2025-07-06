module Language.Comp.CreateVar (createVar) where

import Data.Text qualified as T
import Gensym.Handle
import Language.Common.CreateSymbol
import Language.Common.Ident
import Language.Comp.Comp qualified as C

{-# INLINE createVar #-}
createVar :: Handle -> T.Text -> IO (Ident, C.Value)
createVar h name = do
  x <- newIdentFromText h name
  return (x, C.VarLocal x)
