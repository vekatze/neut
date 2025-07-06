module Language.Common.Attr.Lam
  ( Attr (..),
    normal,
    normal',
    fromAttr,
  )
where

import Data.Binary
import Data.Text qualified as T
import GHC.Generics (Generic)
import Language.Common.Binder
import Language.Common.LamKind

type ID = Int

data Attr a = Attr
  { lamKind :: LamKindF a,
    identity :: ID
  }
  deriving (Show, Generic)

instance (Binary a) => Binary (Attr a)

normal :: ID -> a -> Attr a
normal i codType =
  Attr {lamKind = Normal Nothing codType, identity = i}

normal' :: Maybe T.Text -> ID -> a -> Attr a
normal' name i codType =
  Attr {lamKind = Normal name codType, identity = i}

fromAttr :: Attr a -> Maybe (BinderF a)
fromAttr (Attr {lamKind}) =
  fromLamKind lamKind
