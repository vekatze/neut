module Language.Common.PrimOp.UnaryOp
  ( UnaryOp (..),
    asFloatUnaryOp,
  )
where

import Data.Binary
import Data.Text qualified as T
import GHC.Generics qualified as G

data UnaryOp
  = FNeg
  deriving (Eq, Ord, G.Generic)

instance Binary UnaryOp

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
