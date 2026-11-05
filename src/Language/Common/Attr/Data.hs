module Language.Common.Attr.Data (Attr (..)) where

import Binary.Derive (deriveBinary)
import Language.Common.IsConstLike

newtype Attr = Attr
  { isConstLike :: IsConstLike
  }
  deriving (Show, Eq)

deriveBinary [''Attr]
