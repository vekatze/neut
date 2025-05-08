module Language.Common.Rule.LamKind
  ( LamKindF (..),
    fromLamKind,
  )
where

import Data.Binary
import GHC.Generics
import Language.Common.Rule.Binder

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
