module Entity.WeakPrimValue
  ( WeakPrimValue (..),
  )
where

import Data.Binary
import Data.Text qualified as T
import Entity.PrimOp
import GHC.Generics (Generic)

data WeakPrimValue a
  = Int a Integer
  | Float a Double
  | Op PrimOp
  | StaticText a T.Text
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
      StaticText t text ->
        StaticText (f t) text

instance Foldable WeakPrimValue where
  foldMap f primValue =
    case primValue of
      Int t _ ->
        f t
      Float t _ ->
        f t
      Op _ ->
        mempty
      StaticText t _ ->
        f t

instance Traversable WeakPrimValue where
  traverse f primValue =
    case primValue of
      Int t v ->
        Int <$> f t <*> pure v
      Float t v ->
        Float <$> f t <*> pure v
      Op op ->
        pure $ Op op
      StaticText t text ->
        StaticText <$> f t <*> pure text
