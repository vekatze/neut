module Entity.LamKind where

import Data.Binary
import Entity.Binder
import Entity.Opacity qualified as O
import GHC.Generics

data LamKindF a
  = Normal O.Opacity
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
