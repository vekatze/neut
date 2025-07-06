module Language.Common.LamKind
  ( LamKindF (..),
    fromLamKind,
  )
where

import Data.Binary
import Data.Text qualified as T
import GHC.Generics
import Language.Common.Binder

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
