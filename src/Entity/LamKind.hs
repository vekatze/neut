module Entity.LamKind where

import Data.Binary
import Entity.Binder
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Discriminant as D
import GHC.Generics

data LamKindF a
  = LamKindNormal
  | LamKindCons DD.DefiniteDescription DD.DefiniteDescription D.Discriminant a
  | LamKindFix (BinderF a)
  deriving (Show, Generic)

instance (Binary a) => Binary (LamKindF a)

fromLamKind :: LamKindF a -> Maybe (BinderF a)
fromLamKind k =
  case k of
    LamKindFix x ->
      Just x
    _ ->
      Nothing
