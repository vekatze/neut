module Language.Common.Foreign
  ( BaseForeign (..),
    Foreign,
  )
where

import Data.Binary
import GHC.Generics
import Language.Common.BaseLowType qualified as BLT
import Language.Common.ExternalName qualified as EN
import Language.Common.ForeignCodType
import Logger.Hint

data BaseForeign a
  = Foreign Hint EN.ExternalName [a] (ForeignCodType a)
  deriving (Generic, Functor, Foldable, Traversable)

instance (Binary a) => Binary (BaseForeign a)

type Foreign =
  BaseForeign BLT.BaseLowType
