module Entity.ArrayKind where

import Data.Binary
import Entity.PrimType qualified as PT
import GHC.Generics (Generic)

data ArrayKind a
  = ArrayKindPrimType PT.PrimType
  | ArrayKindGeneral a
  deriving (Show, Generic)

instance Binary a => Binary (ArrayKind a)
