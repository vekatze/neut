module Language.Common.LamKind
  ( LamKindF (..),
    fromLamKind,
  )
where

import Data.Binary
import Data.Text qualified as T
import GHC.Generics
import Language.Common.Binder
import Language.Common.IsDestPassing
import Language.Common.Opacity qualified as O

data LamKindF a
  = Normal (Maybe T.Text) IsDestPassing a
  | Fix O.Opacity IsDestPassing (BinderF a)
  deriving (Show, Generic)

instance (Binary a) => Binary (LamKindF a)

fromLamKind :: LamKindF a -> Maybe (BinderF a)
fromLamKind k =
  case k of
    Fix _ _ x ->
      Just x
    _ ->
      Nothing
