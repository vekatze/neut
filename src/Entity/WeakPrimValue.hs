module Entity.WeakPrimValue
  ( WeakPrimValue (..),
  )
where

import Data.Binary
import GHC.Generics (Generic)

data WeakPrimValue a
  = Int a Integer
  | Float a Double
  deriving (Show, Generic)

instance (Binary a) => Binary (WeakPrimValue a)

instance Functor WeakPrimValue where
  fmap f primValue =
    case primValue of
      Int t v ->
        Int (f t) v
      Float t v ->
        Float (f t) v

instance Foldable WeakPrimValue where
  foldMap f primValue =
    case primValue of
      Int t _ ->
        f t
      Float t _ ->
        f t

instance Traversable WeakPrimValue where
  traverse f primValue =
    case primValue of
      Int t v ->
        Int <$> f t <*> pure v
      Float t v ->
        Float <$> f t <*> pure v
