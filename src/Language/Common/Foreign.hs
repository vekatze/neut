module Language.Common.Foreign
  ( BaseForeign (..),
    ForeignSignature (..),
    Foreign,
    FunctionSignature,
  )
where

import Binary.Derive (deriveBinary)
import Language.Common.BaseLowType qualified as BLT
import Language.Common.ExternalName qualified as EN
import Language.Common.ForeignCodType
import Logger.Hint

data ForeignSignature a
  = Function [a] (ForeignCodType a)
  | Variable a
  deriving (Eq, Functor, Foldable, Traversable)

data BaseForeign a
  = Foreign Hint EN.ExternalName (ForeignSignature a)
  deriving (Functor, Foldable, Traversable)

type Foreign =
  BaseForeign BLT.BaseLowType

type FunctionSignature =
  ([BLT.BaseLowType], ForeignCodType BLT.BaseLowType)

deriveBinary [''ForeignSignature, ''BaseForeign]
