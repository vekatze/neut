module Entity.Term.FromPrimNum (fromPrimNum) where

import Control.Comonad.Cofree
import Entity.Basic
import Entity.PrimNum
import Entity.PrimNumSize.ToText
import Entity.Term

fromPrimNum :: Hint -> PrimNum -> Term
fromPrimNum m primNum =
  case primNum of
    PrimNumInt s ->
      m :< TermConst (intSizeToText s)
    PrimNumFloat s ->
      m :< TermConst (floatSizeToText s)
