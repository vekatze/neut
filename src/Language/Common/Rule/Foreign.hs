module Language.Common.Rule.Foreign
  ( BaseForeign (..),
    Foreign,
  )
where

import Logger.Rule.Hint
import Data.Binary
import GHC.Generics
import Language.Common.Rule.BaseLowType qualified as BLT
import Language.Common.Rule.ExternalName qualified as EN
import Language.Common.Rule.ForeignCodType

data BaseForeign a
  = Foreign Hint EN.ExternalName [a] (ForeignCodType a)
  deriving (Generic, Functor, Foldable, Traversable)

instance (Binary a) => Binary (BaseForeign a)

type Foreign =
  BaseForeign BLT.BaseLowType
