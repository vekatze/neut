module Main.Rule.Attr.Lam
  ( Attr (..),
    normal,
    fromAttr,
  )
where

import Data.Binary
import GHC.Generics (Generic)
import Main.Rule.Binder
import Main.Rule.LamKind

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
