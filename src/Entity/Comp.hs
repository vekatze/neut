module Entity.Comp where

import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import Entity.Arity
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
  | ValueVarGlobal T.Text Arity
  | ValueSigmaIntro [Value]
  | ValueArrayIntro PrimNum [Value]
  | ValueInt IntSize Integer
  | ValueFloat FloatSize Double
  | ValueEnumIntro EnumLabel
  deriving (Show)

data Comp
  = CompPiElimDownElim Value [Value] -- ((force v) v1 ... vn)
  | CompSigmaElim Bool [Ident] Value Comp
  | CompUpIntro Value
  | CompUpElim Ident Comp Comp
  | CompEnumElim Value [(CompEnumCase, Comp)]
  | CompArrayAccess PrimNum Value Value
  | CompPrimitive Primitive
  deriving (Show)

data Primitive
  = PrimitivePrimOp PrimOp [Value]
  | PrimitiveMagic (Magic Value)
  deriving (Show)

type SubstValue =
  IntMap.IntMap Value

type CompDef =
  (T.Text, (Opacity, [Ident], Comp))
