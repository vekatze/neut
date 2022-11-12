module Entity.PrimValue
  ( PrimValue (..),
  )
where

import GHC.Generics (Generic)
import Data.Binary
import Entity.PrimNumSize

data PrimValue
  = Int IntSize Integer
  | Float FloatSize Double
  deriving (Show, Generic)

instance Binary PrimValue

-- instance Functor PrimValue where
--   fmap f primValue =
--     case primValue of
--       Int t v ->
--         Int (f t) v
--       Float t v ->
--         Float (f t) v

-- instance Foldable PrimValue where
--   foldMap f primValue =
--     case primValue of
--       Int t _ ->
--         f t
--       Float t _ ->
--         f t

-- instance Traversable PrimValue where
--   traverse f primValue =
--     case primValue of
--       Int t v ->
--         Int <$> f t <*> pure v
--       Float t v ->
--         Float <$> f t <*> pure v

