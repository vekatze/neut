module Kernel.Common.TypeValue
  ( TypeValue (..),
    Constructor,
    toTypeTag,
    fromIntSize,
    fromFloatSize,
  )
where

import Data.Text qualified as T
import Kernel.Common.TypeTag qualified as TT
import Language.Common.IsConstLike (IsConstLike)
import Language.Common.PrimNumSize
import Language.Term.Term qualified as TM

type ConstructorName =
  T.Text

type ParamName =
  T.Text

type DataName =
  T.Text

type Constructor =
  (ConstructorName, IsConstLike, [(ParamName, TM.Type)])

data TypeValue
  = Opaque
  | Type
  | Function
  | Algebraic DataName [TM.Type] [Constructor]
  | Noema TM.Type
  | Enum [ConstructorName]
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
  | Wrapper TM.Type
  | BoxT TM.Type

toTypeTag :: TypeValue -> TT.TypeTag
toTypeTag tv =
  case tv of
    Opaque ->
      TT.Opaque
    Type ->
      TT.Type
    Function ->
      TT.Function
    Algebraic {} ->
      TT.Algebraic
    Noema _ ->
      TT.Noema
    Enum _ ->
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
    Wrapper _ ->
      TT.Wrapper
    BoxT _ ->
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
