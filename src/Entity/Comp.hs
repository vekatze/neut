module Entity.Comp where

import qualified Data.IntMap as IntMap
import Entity.Arity
import qualified Entity.DefiniteDescription as DD
import Entity.EnumCase
import Entity.Ident
import Entity.Magic
import Entity.Opacity
import Entity.PrimNum
import Entity.PrimNumSize
import Entity.PrimOp

data Value
  = ValueVarLocal Ident
  | ValueVarLocalIdeal Ident
  | ValueVarGlobal DD.DefiniteDescription Arity
  | ValueSigmaIntro [Value]
  | ValueArrayIntro PrimNum [Value]
  | ValueInt IntSize Integer
  | ValueFloat FloatSize Double
  | ValueEnumIntro EnumLabel
  deriving (Show)

data Comp
  = CompPiElimDownElim Value [Value] -- ((force v) v1 ... vn)
  | CompSigmaElim ShouldDeallocate [Ident] Value Comp
  | CompUpIntro Value
  | CompUpElim Ident Comp Comp
  | CompEnumElim Value [(CompEnumCase, Comp)]
  | CompArrayAccess PrimNum Value Value
  | CompPrimitive Primitive
  deriving (Show)

type ShouldDeallocate = Bool

data Primitive
  = PrimitivePrimOp PrimOp [Value]
  | PrimitiveMagic (Magic Value)
  deriving (Show)

type SubstValue =
  IntMap.IntMap Value

type CompDef =
  (DD.DefiniteDescription, (Opacity, [Ident], Comp))
