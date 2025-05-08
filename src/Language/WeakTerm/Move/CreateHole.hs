module Language.WeakTerm.Move.CreateHole (createHole) where

import Control.Comonad.Cofree
import Language.Common.Rule.HoleID (HoleID (HoleID))
import Language.WeakTerm.Rule.WeakTerm
import Library.Gensym.Move.Gensym qualified as Gensym
import Library.Gensym.Rule.Handle qualified as Gensym
import Library.Logger.Rule.Hint

{-# INLINE createHole #-}
createHole :: Gensym.Handle -> Hint -> [WeakTerm] -> IO WeakTerm
createHole h m varSeq = do
  i <- HoleID <$> Gensym.newCount h
  return $ m :< Hole i varSeq
