module Entity.Term.FromPrimNum (fromPrimNum) where

import Control.Comonad.Cofree
import Entity.Hint
import qualified Entity.Prim as Prim
import qualified Entity.PrimNumType as PNT
import qualified Entity.Term as TM

fromPrimNum :: Hint -> PNT.PrimNumType -> TM.Term
fromPrimNum m primNum =
  m :< TM.Prim (Prim.Type primNum)
