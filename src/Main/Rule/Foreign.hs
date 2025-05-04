module Main.Rule.Foreign
  ( BaseForeign (..),
    Foreign,
    defaultForeignList,
  )
where

import Data.Binary
import GHC.Generics
import Main.Rule.Arch qualified as A
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

defaultForeignList :: A.Arch -> [Foreign]
defaultForeignList arch =
  [ Foreign internalHint EN.malloc [BLT.getWordType arch] (Cod BLT.Pointer),
    Foreign internalHint EN.free [BLT.Pointer] Void
  ]
