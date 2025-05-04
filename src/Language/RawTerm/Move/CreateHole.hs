module Language.RawTerm.Move.CreateHole (createHole) where

import Control.Comonad.Cofree
import Gensym.Move.Gensym qualified as Gensym
import Gensym.Rule.Handle qualified as Gensym
import Language.Common.Rule.Hint
import Language.Common.Rule.HoleID (HoleID (HoleID))
import Language.RawTerm.Rule.RawTerm

{-# INLINE createHole #-}
createHole :: Gensym.Handle -> Hint -> IO RawTerm
createHole h m = do
  i <- HoleID <$> Gensym.newCount h
  return $ m :< Hole i
