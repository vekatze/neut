module Language.WeakTerm.CreateHole (createHole) where

import Control.Comonad.Cofree
import Gensym.Gensym qualified as Gensym
import Gensym.Handle qualified as Gensym
import Language.Common.HoleID (HoleID (HoleID))
import Language.WeakTerm.WeakTerm
import Logger.Hint

{-# INLINE createHole #-}
createHole :: Gensym.Handle -> Hint -> [WeakTerm] -> IO WeakTerm
createHole h m varSeq = do
  i <- HoleID <$> Gensym.newCount h
  return $ m :< Hole i varSeq
