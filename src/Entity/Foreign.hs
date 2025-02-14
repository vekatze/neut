module Entity.Foreign
  ( BaseForeign (..),
    Foreign,
    WeakForeign,
    defaultForeignList,
    defaultWeakForeignList,
  )
where

import Data.Binary
import Entity.Arch qualified as A
import Entity.BaseLowType qualified as BLT
import Entity.ExternalName qualified as EN
import Entity.ForeignCodType
import Entity.Hint
import Entity.WeakTerm qualified as WT
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
