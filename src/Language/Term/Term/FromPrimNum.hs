module Language.Term.Term.FromPrimNum (fromPrimNum) where

import Control.Comonad.Cofree
import Language.Common.PrimType qualified as PT
import Language.Term.Prim qualified as Prim
import Language.Term.Term qualified as TM
import Logger.Hint

fromPrimNum :: Hint -> PT.PrimType -> TM.Term
fromPrimNum m primNum =
  m :< TM.Prim (Prim.Type primNum)
