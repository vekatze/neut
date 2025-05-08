module Language.RawTerm.Move.CreateHole (createHole) where

import Control.Comonad.Cofree
import Language.Common.Rule.HoleID (HoleID (HoleID))
import Language.RawTerm.Rule.RawTerm
import Library.Gensym.Move.Gensym qualified as Gensym
import Library.Gensym.Rule.Handle qualified as Gensym
import Library.Logger.Rule.Hint

{-# INLINE createHole #-}
createHole :: Gensym.Handle -> Hint -> IO RawTerm
createHole h m = do
  i <- HoleID <$> Gensym.newCount h
  return $ m :< Hole i
