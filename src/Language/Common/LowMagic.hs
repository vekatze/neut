module Language.Common.LowMagic (LowMagic (..)) where

import Binary.Derive (deriveBinary)
import Language.Common.ExternalName qualified as EN
import Language.Common.ForeignCodType qualified as FCT

-- lt: lowtype, ty: type expression, a: term
data LowMagic lt ty a
  = Cast ty ty a -- Cast fromType toType value
  | Store lt ty a a -- Store valueType unitType value pointer
  | Load lt a -- Load valueType pointer
  | Shift a a -- Shift pointer offset
  | Alloca lt a -- Alloca elemType size
  | External [lt] (FCT.ForeignCodType lt) EN.ExternalName [a] [(a, lt)]
  | Global EN.ExternalName lt
  | OpaqueValue a
  | CallType a a a a
  deriving (Show, Eq, Functor, Foldable, Traversable)

deriveBinary [''LowMagic]
