module Rule.Attr.Lam
  ( Attr (..),
    normal,
    fromAttr,
  )
where

import Data.Binary
import Rule.Binder
import Rule.LamKind
import GHC.Generics (Generic)

type ID = Int

data Attr a = Attr
  { lamKind :: LamKindF a,
    identity :: ID
  }
  deriving (Show, Generic)

instance (Binary a) => Binary (Attr a)

normal :: ID -> a -> Attr a
normal i codType =
  Attr {lamKind = Normal codType, identity = i}

fromAttr :: Attr a -> Maybe (BinderF a)
fromAttr (Attr {lamKind}) =
  fromLamKind lamKind
