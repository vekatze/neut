module Entity.Term.FromPrimNum (fromPrimNum) where

import Control.Comonad.Cofree
import Entity.Hint
import Entity.Prim qualified as Prim
import Entity.PrimType qualified as PT
import Entity.Term qualified as TM

fromPrimNum :: Hint -> PT.PrimType -> TM.Term
fromPrimNum m primNum =
  m :< TM.Prim (Prim.Type primNum)
