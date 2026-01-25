module Kernel.Common.TypeTag
  ( TypeTag (..),
    typeTagToInteger,
    typeTagList,
  )
where

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
  | Wrapper
  | BoxT

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
    Wrapper ->
      21
    BoxT ->
      22

typeTagList :: [TypeTag]
typeTagList =
  [ Opaque,
    Type,
    Function,
    Algebraic,
    Noema,
    Enum,
    Int1,
    Int2,
    Int4,
    Int8,
    Int16,
    Int32,
    Int64,
    Float16,
    Float32,
    Float64,
    Pointer,
    Null,
    Rune,
    Binary,
    Vector,
    Wrapper,
    BoxT
  ]
