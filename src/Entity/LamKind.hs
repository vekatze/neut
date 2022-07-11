module Entity.LamKind where

import Data.Binary
import qualified Data.Text as T
import Entity.Binder
import qualified Entity.DefiniteDescription as DD
import GHC.Generics

type DataName =
  T.Text

type ConsName =
  T.Text

type ConsNumber =
  Integer

data LamKindF a
  = LamKindNormal
  | LamKindCons DD.DefiniteDescription DD.DefiniteDescription ConsNumber a
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
