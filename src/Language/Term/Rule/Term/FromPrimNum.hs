module Language.Term.Rule.Term.FromPrimNum (fromPrimNum) where

import Logger.Rule.Hint
import Control.Comonad.Cofree
import Language.Common.Rule.PrimType qualified as PT
import Language.Term.Rule.Prim qualified as Prim
import Language.Term.Rule.Term qualified as TM

fromPrimNum :: Hint -> PT.PrimType -> TM.Term
fromPrimNum m primNum =
  m :< TM.Prim (Prim.Type primNum)
