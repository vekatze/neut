module Entity.Attr.Lam
  ( Attr (..),
    normal,
    fromAttr,
  )
where

import Data.Binary
import Entity.Binder
import Entity.LamKind
import GHC.Generics (Generic)

newtype Attr a = Attr
  { lamKind :: LamKindF a
  }
  deriving (Show, Generic)

instance (Binary a) => Binary (Attr a)

normal :: Attr a
normal =
  Attr {lamKind = Normal}

fromAttr :: Attr a -> Maybe (BinderF a)
fromAttr (Attr {lamKind}) =
  fromLamKind lamKind
