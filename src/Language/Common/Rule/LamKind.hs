module Language.Common.Rule.LamKind
  ( LamKindF (..),
    fromLamKind,
  )
where

import Data.Binary
import Data.Text qualified as T
import GHC.Generics
import Language.Common.Rule.Binder

data LamKindF a
  = Normal (Maybe T.Text) a
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
