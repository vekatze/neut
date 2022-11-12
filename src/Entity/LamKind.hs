module Entity.LamKind where

import Data.Binary
import Entity.Binder
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Discriminant as D
import GHC.Generics

data LamKindF a
  = Normal
  | Cons DD.DefiniteDescription DD.DefiniteDescription D.Discriminant a
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
