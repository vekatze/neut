module Entity.Term.FromPrimNum (fromPrimNum) where

import Control.Comonad.Cofree
import Entity.Hint
import qualified Entity.Prim as Prim
import Entity.PrimNum
import qualified Entity.Term as TM

fromPrimNum :: Hint -> PrimNum -> TM.Term
fromPrimNum m primNum =
  m :< TM.Prim (Prim.Type primNum)
