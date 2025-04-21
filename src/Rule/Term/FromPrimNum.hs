module Rule.Term.FromPrimNum (fromPrimNum) where

import Control.Comonad.Cofree
import Rule.Hint
import Rule.Prim qualified as Prim
import Rule.PrimType qualified as PT
import Rule.Term qualified as TM

fromPrimNum :: Hint -> PT.PrimType -> TM.Term
fromPrimNum m primNum =
  m :< TM.Prim (Prim.Type primNum)
