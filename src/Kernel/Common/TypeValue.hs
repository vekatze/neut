module Kernel.Common.TypeValue
  ( TypeValue (..),
    toTypeTag,
    fromIntSize,
    fromFloatSize,
  )
where

import Kernel.Common.TypeTag qualified as TT
import Language.Common.PrimNumSize
import Language.Term.Term qualified as TM

data TypeValue
  = Opaque
  | Type
  | Function
  | Algebraic
  | Noema
  | Enum
  | Int1
  | Int2
  | Int4
  | Int8
  | Int16
  | Int32
  | Int64
  | Float16
  | Float32
  | Float64
  | Pointer
  | Null
  | Rune
  | Binary
  | Vector TM.Type
  | Wrapper
  | BoxT

toTypeTag :: TypeValue -> TT.TypeTag
toTypeTag tv =
  case tv of
    Opaque ->
      TT.Opaque
    Type ->
      TT.Type
    Function ->
      TT.Function
    Algebraic ->
      TT.Algebraic
    Noema ->
      TT.Noema
    Enum ->
      TT.Enum
    Int1 ->
      TT.Int1
    Int2 ->
      TT.Int2
    Int4 ->
      TT.Int4
    Int8 ->
      TT.Int8
    Int16 ->
      TT.Int16
    Int32 ->
      TT.Int32
    Int64 ->
      TT.Int64
    Float16 ->
      TT.Float16
    Float32 ->
      TT.Float32
    Float64 ->
      TT.Float64
    Pointer ->
      TT.Pointer
    Null ->
      TT.Null
    Rune ->
      TT.Rune
    Binary ->
      TT.Binary
    Vector _ ->
      TT.Vector
    Wrapper ->
      TT.Wrapper
    BoxT ->
      TT.BoxT

fromIntSize :: IntSize -> TypeValue
fromIntSize s =
  case s of
    IntSize1 ->
      Int1
    IntSize2 ->
      Int2
    IntSize4 ->
      Int4
    IntSize8 ->
      Int8
    IntSize16 ->
      Int16
    IntSize32 ->
      Int32
    IntSize64 ->
      Int64

fromFloatSize :: FloatSize -> TypeValue
fromFloatSize s =
  case s of
    FloatSize16 ->
      Float16
    FloatSize32 ->
      Float32
    FloatSize64 ->
      Float64
