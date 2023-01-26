module Entity.ArrayKind where

import Data.Binary
import Entity.PrimType qualified as PT
import GHC.Generics (Generic)

data ArrayKind a
  = PrimType PT.PrimType
  | General a
  deriving (Show, Generic, Traversable, Functor, Foldable)

instance Binary a => Binary (ArrayKind a)

type IsVector =
  Bool
