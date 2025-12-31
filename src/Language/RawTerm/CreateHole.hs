module Language.RawTerm.CreateHole (createTypeHole) where

import Control.Comonad.Cofree
import Gensym.Gensym qualified as Gensym
import Gensym.Handle qualified as Gensym
import Language.Common.HoleID (HoleID (HoleID))
import Language.RawTerm.RawTerm
import Logger.Hint

{-# INLINE createTypeHole #-}
createTypeHole :: Gensym.Handle -> Hint -> IO RawType
createTypeHole h m = do
  i <- HoleID <$> Gensym.newCount h
  return $ m :< TypeHole i
