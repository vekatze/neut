module Language.Common.PrimOp.UnaryOp
  ( UnaryOp (..),
    asFloatUnaryOp,
  )
where

import Binary.Derive (deriveBinary)
import Data.Text qualified as T

data UnaryOp
  = FNeg
  deriving (Eq, Ord)

instance Show UnaryOp where
  show op =
    case op of
      FNeg ->
        "fneg"

asFloatUnaryOp :: T.Text -> Maybe UnaryOp
asFloatUnaryOp name =
  case name of
    "neg" ->
      return FNeg
    _ ->
      Nothing

deriveBinary [''UnaryOp]
