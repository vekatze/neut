module Kernel.Common.TypeTag
  ( TypeTag (..),
    typeTagToInteger,
    immTypeTagMap,
    baseTypeTagMap,
  )
where

import Language.Common.DefiniteDescription qualified as DD

data TypeTag
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
  | Vector

immTypeTagMap :: [(DD.DefiniteDescription, TypeTag)]
immTypeTagMap =
  [ (DD.immType, Type),
    (DD.immNoema, Noema),
    (DD.immInt1, Int1),
    (DD.immInt2, Int2),
    (DD.immInt4, Int4),
    (DD.immInt8, Int8),
    (DD.immInt16, Int16),
    (DD.immInt32, Int32),
    (DD.immInt64, Int64),
    (DD.immFloat16, Float16),
    (DD.immFloat32, Float32),
    (DD.immFloat64, Float64),
    (DD.immPointer, Pointer),
    (DD.immNull, Null),
    (DD.immRune, Rune)
  ]

baseTypeTagMap :: [(DD.DefiniteDescription, TypeTag)]
baseTypeTagMap =
  immTypeTagMap ++ [(DD.cls, Function)]

typeTagToInteger :: TypeTag -> Integer
typeTagToInteger tag =
  case tag of
    Opaque ->
      0
    Type ->
      1
    Function ->
      2
    Algebraic ->
      3
    Noema ->
      4
    Enum ->
      5
    Int1 ->
      6
    Int2 ->
      7
    Int4 ->
      8
    Int8 ->
      9
    Int16 ->
      10
    Int32 ->
      11
    Int64 ->
      12
    Float16 ->
      13
    Float32 ->
      14
    Float64 ->
      15
    Pointer ->
      16
    Null ->
      17
    Rune ->
      18
    Binary ->
      19
    Vector ->
      20
