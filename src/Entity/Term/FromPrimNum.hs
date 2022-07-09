module Entity.Term.FromPrimNum (fromPrimNum) where

import Control.Comonad.Cofree
import Entity.Hint
import qualified Entity.Prim as Prim
import Entity.PrimNum
import Entity.Term

fromPrimNum :: Hint -> PrimNum -> Term
fromPrimNum m primNum =
  m :< TermPrim (Prim.Type primNum)
