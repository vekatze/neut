module Main.Rule.PrimOp.BinaryOp
  ( BinaryOp (..),
    asIntBinaryOp,
    asFloatBinaryOp,
  )
where

import Data.Binary
import Data.Text qualified as T
import GHC.Generics qualified as G

data BinaryOp
  = Add
  | Sub
  | Mul
  | SDiv
  | SRem
  | UDiv
  | URem
  | And
  | Or
  | Xor
  | Shl
  | Lshr
  | Ashr
  | FAdd
  | FSub
  | FMul
  | FDiv
  | FRem
  deriving (Eq, Ord, G.Generic)

instance Binary BinaryOp

instance Show BinaryOp where
  show op =
    case op of
      Add ->
        "add"
      Sub ->
        "sub"
      Mul ->
        "mul"
      SDiv ->
        "sdiv"
      SRem ->
        "srem"
      UDiv ->
        "udiv"
      URem ->
        "urem"
      And ->
        "and"
      Or ->
        "or"
      Xor ->
        "xor"
      Shl ->
        "shl"
      Lshr ->
        "lshr"
      Ashr ->
        "ashr"
      FAdd ->
        "fadd"
      FSub ->
        "fsub"
      FMul ->
        "fmul"
      FDiv ->
        "fdiv"
      FRem ->
        "frem"

asIntBinaryOp :: T.Text -> Maybe BinaryOp
asIntBinaryOp name =
  case name of
    "add" ->
      return Add
    "sub" ->
      return Sub
    "mul" ->
      return Mul
    "div" ->
      return SDiv
    "rem" ->
      return SRem
    "udiv" ->
      return UDiv
    "urem" ->
      return URem
    "and" ->
      return And
    "or" ->
      return Or
    "xor" ->
      return Xor
    "shl" ->
      return Shl
    "lshr" ->
      return Lshr
    "ashr" ->
      return Ashr
    _ ->
      Nothing

asFloatBinaryOp :: T.Text -> Maybe BinaryOp
asFloatBinaryOp name =
  case name of
    "add" ->
      return FAdd
    "sub" ->
      return FSub
    "mul" ->
      return FMul
    "div" ->
      return FDiv
    "rem" ->
      return FRem
    _ ->
      Nothing
