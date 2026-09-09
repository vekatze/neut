module Language.Common.Foreign
  ( BaseForeign (..),
    ForeignSignature (..),
    Foreign,
  )
where

import Data.Binary
import GHC.Generics
import Language.Common.BaseLowType qualified as BLT
import Language.Common.ExternalName qualified as EN
import Language.Common.ForeignCodType
import Logger.Hint

data ForeignSignature a
  = Function [a] (ForeignCodType a)
  | Variable a
  deriving (Generic, Eq, Functor, Foldable, Traversable)

instance (Binary a) => Binary (ForeignSignature a)

data BaseForeign a
  = Foreign Hint EN.ExternalName (ForeignSignature a)
  deriving (Generic, Functor, Foldable, Traversable)

instance (Binary a) => Binary (BaseForeign a)

type Foreign =
  BaseForeign BLT.BaseLowType
