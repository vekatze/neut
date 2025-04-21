module Rule.PrimOp (PrimOp (..), getTypeInfo) where

import Data.Binary
import Rule.PrimOp.BinaryOp
import Rule.PrimOp.CmpOp
import Rule.PrimOp.ConvOp
import Rule.PrimOp.UnaryOp
import Rule.PrimType qualified as PT
import GHC.Generics qualified as G

data PrimOp
  = PrimUnaryOp UnaryOp PT.PrimType PT.PrimType
  | PrimBinaryOp BinaryOp PT.PrimType PT.PrimType
  | PrimCmpOp CmpOp PT.PrimType PT.PrimType
  | PrimConvOp ConvOp PT.PrimType PT.PrimType
  deriving (Show, Eq, Ord, G.Generic)

instance Binary PrimOp

getTypeInfo :: PrimOp -> ([PT.PrimType], PT.PrimType)
getTypeInfo op =
  case op of
    PrimUnaryOp _ dom cod ->
      ([dom], cod)
    PrimBinaryOp _ dom cod ->
      ([dom, dom], cod)
    PrimCmpOp _ dom cod ->
      ([dom, dom], cod)
    PrimConvOp _ dom cod ->
      ([dom], cod)
