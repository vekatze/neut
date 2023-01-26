module Entity.WeakArrayKind where

import Data.Binary
import GHC.Generics (Generic)

data WeakArrayKind a
  = PrimType a
  | General a
  deriving (Show, Generic, Traversable, Functor, Foldable)

instance Binary a => Binary (WeakArrayKind a)

type IsVector =
  Bool

extract :: WeakArrayKind a -> a
extract ak =
  case ak of
    PrimType t ->
      t
    General t ->
      t
