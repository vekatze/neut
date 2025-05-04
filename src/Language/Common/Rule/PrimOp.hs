module Language.Common.Rule.PrimOp (PrimOp (..), getTypeInfo) where

import Data.Binary
import GHC.Generics qualified as G
import Language.Common.Rule.PrimOp.BinaryOp
import Language.Common.Rule.PrimOp.CmpOp
import Language.Common.Rule.PrimOp.ConvOp
import Language.Common.Rule.PrimOp.UnaryOp
import Language.Common.Rule.PrimType qualified as PT

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
