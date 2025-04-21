module Rule.LamKind
  ( LamKindF (..),
    fromLamKind,
  )
where

import Data.Binary
import Rule.Binder
import GHC.Generics

data LamKindF a
  = Normal a
  | Fix (BinderF a)
  deriving (Show, Generic)

instance (Binary a) => Binary (LamKindF a)

fromLamKind :: LamKindF a -> Maybe (BinderF a)
fromLamKind k =
  case k of
    Fix x ->
      Just x
    _ ->
      Nothing
