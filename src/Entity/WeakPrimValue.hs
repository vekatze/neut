module Entity.WeakPrimValue
  ( WeakPrimValue (..),
  )
where

import Data.Binary
import Entity.PrimOp
import GHC.Generics (Generic)

data WeakPrimValue a
  = Int a Integer
  | Float a Double
  | Op PrimOp
  deriving (Show, Generic)

instance (Binary a) => Binary (WeakPrimValue a)

instance Functor WeakPrimValue where
  fmap f primValue =
    case primValue of
      Int t v ->
        Int (f t) v
      Float t v ->
        Float (f t) v
      Op op ->
        Op op

instance Foldable WeakPrimValue where
  foldMap f primValue =
    case primValue of
      Int t _ ->
        f t
      Float t _ ->
        f t
      Op _ ->
        mempty

instance Traversable WeakPrimValue where
  traverse f primValue =
    case primValue of
      Int t v ->
        Int <$> f t <*> pure v
      Float t v ->
        Float <$> f t <*> pure v
      Op op ->
        pure $ Op op
