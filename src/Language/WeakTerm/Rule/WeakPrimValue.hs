module Language.WeakTerm.Rule.WeakPrimValue
  ( WeakPrimValue (..),
    reflectFloatUnaryOp,
    reflectIntegerBinaryOp,
    reflectFloatBinaryOp,
    reflectIntegerCmpOp,
    reflectFloatCmpOp,
  )
where

import Data.Binary
import Data.Text qualified as T
import GHC.Generics (Generic)
import Language.Common.Rule.PrimOp
import Language.Common.Rule.PrimOp.BinaryOp
import Language.Common.Rule.PrimOp.CmpOp
import Language.Common.Rule.PrimOp.UnaryOp
import Language.Common.Rule.PrimType (PrimType)
import Language.Common.Rule.Rune qualified as RU

data WeakPrimValue a
  = Int a Integer
  | Float a Double
  | Op PrimOp
  | StaticText a T.Text
  | Rune RU.Rune
  deriving (Show, Generic)

instance (Binary a) => Binary (WeakPrimValue a)

instance Functor WeakPrimValue where
  fmap f primValue =
    case primValue of
      Int t v ->
        Int (f t) v
      Float t v ->
        Float (f t) v
      Op op ->
        Op op
      StaticText t text ->
        StaticText (f t) text
      Rune r ->
        Rune r

instance Foldable WeakPrimValue where
  foldMap f primValue =
    case primValue of
      Int t _ ->
        f t
      Float t _ ->
        f t
      Op _ ->
        mempty
      StaticText t _ ->
        f t
      Rune _ ->
        mempty

instance Traversable WeakPrimValue where
  traverse f primValue =
    case primValue of
      Int t v ->
        Int <$> f t <*> pure v
      Float t v ->
        Float <$> f t <*> pure v
      Op op ->
        pure $ Op op
      StaticText t text ->
        StaticText <$> f t <*> pure text
      Rune r ->
        pure $ Rune r

reflectFloatUnaryOp :: PrimOp -> Maybe (Double -> Double, PrimType)
reflectFloatUnaryOp primOp =
  case primOp of
    PrimUnaryOp FNeg _ cod ->
      return (negate, cod)
    _ ->
      Nothing

reflectIntegerBinaryOp :: PrimOp -> Maybe (Integer -> Integer -> Integer, PrimType)
reflectIntegerBinaryOp primOp =
  case primOp of
    PrimBinaryOp Add _ cod ->
      return ((+), cod)
    PrimBinaryOp Sub _ cod ->
      return ((-), cod)
    PrimBinaryOp Mul _ cod ->
      return ((*), cod)
    _ ->
      Nothing

reflectFloatBinaryOp :: PrimOp -> Maybe (Double -> Double -> Double, PrimType)
reflectFloatBinaryOp primOp =
  case primOp of
    PrimBinaryOp FAdd _ cod ->
      return ((+), cod)
    PrimBinaryOp FSub _ cod ->
      return ((-), cod)
    PrimBinaryOp FMul _ cod ->
      return ((*), cod)
    PrimBinaryOp FDiv _ cod ->
      return ((/), cod)
    _ ->
      Nothing

reflectIntegerCmpOp :: PrimOp -> Maybe (Integer -> Integer -> Integer, PrimType)
reflectIntegerCmpOp primOp =
  case primOp of
    PrimCmpOp Eq _ cod ->
      return (\x y -> boolToInteger (x == y), cod)
    PrimCmpOp Ne _ cod ->
      return (\x y -> boolToInteger (x /= y), cod)
    PrimCmpOp SGt _ cod ->
      return (\x y -> boolToInteger (x > y), cod)
    PrimCmpOp SGe _ cod ->
      return (\x y -> boolToInteger (x >= y), cod)
    PrimCmpOp SLt _ cod ->
      return (\x y -> boolToInteger (x < y), cod)
    PrimCmpOp SLe _ cod ->
      return (\x y -> boolToInteger (x <= y), cod)
    _ ->
      Nothing

reflectFloatCmpOp :: PrimOp -> Maybe (Double -> Double -> Integer, PrimType)
reflectFloatCmpOp primOp =
  case primOp of
    PrimCmpOp FOEq _ cod ->
      return (\x y -> boolToInteger (x == y), cod)
    PrimCmpOp FONe _ cod ->
      return (\x y -> boolToInteger (x /= y), cod)
    PrimCmpOp FOGt _ cod ->
      return (\x y -> boolToInteger (x > y), cod)
    PrimCmpOp FOGe _ cod ->
      return (\x y -> boolToInteger (x >= y), cod)
    PrimCmpOp FOLt _ cod ->
      return (\x y -> boolToInteger (x < y), cod)
    PrimCmpOp FOLe _ cod ->
      return (\x y -> boolToInteger (x <= y), cod)
    _ ->
      Nothing

boolToInteger :: Bool -> Integer
boolToInteger b =
  if b
    then 1
    else 0
