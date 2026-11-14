module Kernel.Common.TypeTag
  ( TypeTag (..),
    typeTagToInteger,
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
  | Array
  | Wrapper
  | BoxT
  deriving (Enum, Bounded)

typeTagToInteger :: TypeTag -> Integer
typeTagToInteger =
  fromIntegral . fromEnum
