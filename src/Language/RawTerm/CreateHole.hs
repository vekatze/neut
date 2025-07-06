module Language.RawTerm.CreateHole (createHole) where

import Control.Comonad.Cofree
import Gensym.Gensym qualified as Gensym
import Gensym.Handle qualified as Gensym
import Language.Common.HoleID (HoleID (HoleID))
import Language.RawTerm.RawTerm
import Logger.Hint

{-# INLINE createHole #-}
createHole :: Gensym.Handle -> Hint -> IO RawTerm
createHole h m = do
  i <- HoleID <$> Gensym.newCount h
  return $ m :< Hole i
