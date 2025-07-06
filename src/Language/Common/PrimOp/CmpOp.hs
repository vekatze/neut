module Language.Common.PrimOp.CmpOp
  ( CmpOp (..),
    asIntCmpOp,
    asFloatCmpOp,
  )
where

import Data.Binary
import Data.Text qualified as T
import GHC.Generics qualified as G

data CmpOp
  = Eq
  | Ne
  | SGt
  | SGe
  | SLt
  | SLe
  | UGt
  | UGe
  | ULt
  | ULe
  | FOEq
  | FONe
  | FOGt
  | FOGe
  | FOLt
  | FOLe
  | FUEq
  | FUGt
  | FUGe
  | FULt
  | FULe
  | FUNe
  | FOrd
  | FUno
  | FTrue
  | FFalse
  deriving (Eq, Ord, G.Generic)

instance Binary CmpOp

icmp :: String -> String
icmp cmp =
  "icmp " ++ cmp

fcmp :: String -> String
fcmp cmp =
  "fcmp " ++ cmp

instance Show CmpOp where
  show op =
    case op of
      Eq ->
        icmp "eq"
      Ne ->
        icmp "ne"
      SGt ->
        icmp "sgt"
      SGe ->
        icmp "sge"
      SLt ->
        icmp "slt"
      SLe ->
        icmp "sle"
      UGt ->
        icmp "ugt"
      UGe ->
        icmp "uge"
      ULt ->
        icmp "ult"
      ULe ->
        icmp "ule"
      FOEq ->
        fcmp "oeq"
      FONe ->
        fcmp "one"
      FOGt ->
        fcmp "ogt"
      FOGe ->
        fcmp "oge"
      FOLt ->
        fcmp "olt"
      FOLe ->
        fcmp "ole"
      FUEq ->
        fcmp "ueq"
      FUNe ->
        fcmp "une"
      FUGt ->
        fcmp "ugt"
      FUGe ->
        fcmp "uge"
      FULt ->
        fcmp "ult"
      FULe ->
        fcmp "ule"
      FOrd ->
        fcmp "ord"
      FUno ->
        fcmp "uno"
      FTrue ->
        fcmp "true"
      FFalse ->
        fcmp "false"

asIntCmpOp :: T.Text -> Maybe CmpOp
asIntCmpOp name =
  case name of
    "eq" ->
      return Eq
    "ne" ->
      return Ne
    "gt" ->
      return SGt
    "ge" ->
      return SGe
    "lt" ->
      return SLt
    "le" ->
      return SLe
    "ugt" ->
      return UGt
    "uge" ->
      return UGe
    "ult" ->
      return ULt
    "ule" ->
      return ULe
    _ ->
      Nothing

asFloatCmpOp :: T.Text -> Maybe CmpOp
asFloatCmpOp name =
  case name of
    "eq" ->
      return FOEq
    "ne" ->
      return FONe
    "gt" ->
      return FOGt
    "ge" ->
      return FOGe
    "lt" ->
      return FOLt
    "le" ->
      return FOLe
    "ueq" ->
      return FUEq
    "une" ->
      return FUNe
    "ugt" ->
      return FUGt
    "uge" ->
      return FUGe
    "ult" ->
      return FULt
    "ule" ->
      return FULe
    "ord" ->
      return FOrd
    "uno" ->
      return FUno
    "true" ->
      return FTrue
    "false" ->
      return FFalse
    _ ->
      Nothing
