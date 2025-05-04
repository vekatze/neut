module Main.Rule.Foreign
  ( BaseForeign (..),
    Foreign,
  )
where

import Data.Binary
import GHC.Generics
import Main.Rule.BaseLowType qualified as BLT
import Main.Rule.ExternalName qualified as EN
import Main.Rule.ForeignCodType
import Main.Rule.Hint

data BaseForeign a
  = Foreign Hint EN.ExternalName [a] (ForeignCodType a)
  deriving (Generic, Functor, Foldable, Traversable)

instance (Binary a) => Binary (BaseForeign a)

type Foreign =
  BaseForeign BLT.BaseLowType
