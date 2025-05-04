module Language.WeakTerm.Move.CreateHole (createHole) where

import Control.Comonad.Cofree
import Gensym.Move.Gensym qualified as Gensym
import Gensym.Rule.Handle qualified as Gensym
import Language.Common.Rule.Hint
import Language.Common.Rule.HoleID (HoleID (HoleID))
import Language.WeakTerm.Rule.WeakTerm

{-# INLINE createHole #-}
createHole :: Gensym.Handle -> Hint -> [WeakTerm] -> IO WeakTerm
createHole h m varSeq = do
  i <- HoleID <$> Gensym.newCount h
  return $ m :< Hole i varSeq
