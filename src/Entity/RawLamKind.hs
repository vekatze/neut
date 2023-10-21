module Entity.RawLamKind where

import Data.Binary
import Entity.RawBinder
import GHC.Generics

data RawLamKind a
  = Normal
  | Fix (RawBinder a)
  deriving (Show, Generic)

instance (Binary a) => Binary (RawLamKind a)

fromRawLamKind :: RawLamKind a -> Maybe (RawBinder a)
fromRawLamKind k =
  case k of
    Fix x ->
      Just x
    _ ->
      Nothing
