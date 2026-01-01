module Language.WeakTerm.CreateHole
  ( createTypeHole,
  )
where

import Control.Comonad.Cofree
import Gensym.Gensym qualified as Gensym
import Gensym.Handle qualified as Gensym
import Language.Common.HoleID (HoleID (HoleID))
import Language.WeakTerm.WeakTerm
import Logger.Hint

createTypeHole :: Gensym.Handle -> Hint -> [WeakType] -> IO WeakType
createTypeHole h m varSeq = do
  i <- HoleID <$> Gensym.newCount h
  return $ m :< TypeHole i varSeq
