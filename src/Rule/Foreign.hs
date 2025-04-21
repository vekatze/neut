module Rule.Foreign
  ( BaseForeign (..),
    Foreign,
    WeakForeign,
    defaultForeignList,
    defaultWeakForeignList,
  )
where

import Data.Binary
import Rule.Arch qualified as A
import Rule.BaseLowType qualified as BLT
import Rule.ExternalName qualified as EN
import Rule.ForeignCodType
import Rule.Hint
import Rule.WeakTerm qualified as WT
import GHC.Generics

data BaseForeign a
  = Foreign Hint EN.ExternalName [a] (ForeignCodType a)
  deriving (Generic, Functor, Foldable, Traversable)

instance (Binary a) => Binary (BaseForeign a)

type Foreign =
  BaseForeign BLT.BaseLowType

type WeakForeign =
  BaseForeign WT.WeakTerm

defaultForeignList :: A.Arch -> [Foreign]
defaultForeignList arch =
  [ Foreign internalHint EN.malloc [BLT.getWordType arch] (Cod BLT.Pointer),
    Foreign internalHint EN.free [BLT.Pointer] Void
  ]

defaultWeakForeignList :: A.Arch -> [WeakForeign]
defaultWeakForeignList arch =
  fmap (fmap (WT.fromBaseLowType internalHint)) (defaultForeignList arch)
