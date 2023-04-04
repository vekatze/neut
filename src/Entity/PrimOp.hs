module Entity.PrimOp where

import Data.Binary
import Data.Text qualified as T
import Entity.PrimNumSize
import Entity.PrimType qualified as PT
import GHC.Generics qualified as G

data PrimOp
  = PrimUnaryOp T.Text PT.PrimType PT.PrimType
  | PrimBinaryOp T.Text PT.PrimType PT.PrimType
  | PrimCmpOp T.Text PT.PrimType PT.PrimType
  | PrimConvOp T.Text PT.PrimType PT.PrimType
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

unaryOp :: T.Text -> PT.PrimType -> PrimOp
unaryOp name primType =
  PrimUnaryOp name primType primType

binOp :: T.Text -> PT.PrimType -> PrimOp
binOp name primType =
  PrimBinaryOp name primType primType

cmpOp :: T.Text -> PT.PrimType -> PrimOp
cmpOp name primType =
  case primType of
    PT.Float {} ->
      PrimCmpOp ("fcmp " <> name) primType (PT.Int $ IntSize 1)
    _ ->
      PrimCmpOp ("icmp " <> name) primType (PT.Int $ IntSize 1)
