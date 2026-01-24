module Language.Common.LamKind
  ( LamKindF (..),
    fromLamKind,
  )
where

import Data.Binary
import Data.Text qualified as T
import GHC.Generics
import Language.Common.Binder
import Language.Common.Opacity qualified as O

data LamKindF a
  = Normal (Maybe T.Text) a
  | Fix O.Opacity (BinderF a)
  deriving (Show, Generic)

instance (Binary a) => Binary (LamKindF a)

fromLamKind :: LamKindF a -> Maybe (BinderF a)
fromLamKind k =
  case k of
    Fix _ x ->
      Just x
    _ ->
      Nothing
