module Main.Rule.Term.FromPrimNum (fromPrimNum) where

import Control.Comonad.Cofree
import Main.Rule.Hint
import Main.Rule.Prim qualified as Prim
import Main.Rule.PrimType qualified as PT
import Main.Rule.Term qualified as TM

fromPrimNum :: Hint -> PT.PrimType -> TM.Term
fromPrimNum m primNum =
  m :< TM.Prim (Prim.Type primNum)
