module Language.Term.FromPrimNum (fromPrimNum) where

import Control.Comonad.Cofree
import Language.Common.PrimType qualified as PT
import Language.Term.Term qualified as TM
import Logger.Hint

fromPrimNum :: Hint -> PT.PrimType -> TM.Type
fromPrimNum m primNum =
  m :< TM.PrimType primNum
