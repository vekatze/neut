module Entity.Term.FromPrimNum (fromPrimNum) where

import Control.Comonad.Cofree
import Entity.Hint
import qualified Entity.Prim as Prim
import qualified Entity.PrimType as PT
import qualified Entity.Term as TM

fromPrimNum :: Hint -> PT.PrimType -> TM.Term
fromPrimNum m primNum =
  m :< TM.Prim (Prim.Type primNum)
