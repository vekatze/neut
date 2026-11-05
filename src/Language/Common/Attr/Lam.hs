module Language.Common.Attr.Lam
  ( Attr (..),
    normal,
    normal',
    fromAttr,
  )
where

import Binary.Derive (deriveBinary)
import Data.Text qualified as T
import Language.Common.Binder
import Language.Common.LamKind

type ID = Int

data Attr a = Attr
  { lamKind :: LamKindF a,
    identity :: ID
  }
  deriving (Show)

normal :: ID -> a -> Attr a
normal i codType =
  Attr {lamKind = Normal Nothing False codType, identity = i}

normal' :: Maybe T.Text -> Bool -> ID -> a -> Attr a
normal' name isDestPassing i codType =
  Attr {lamKind = Normal name isDestPassing codType, identity = i}

fromAttr :: Attr a -> Maybe (BinderF a)
fromAttr (Attr {lamKind}) =
  fromLamKind lamKind

deriveBinary [''Attr]
