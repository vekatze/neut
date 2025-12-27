module Language.Common.Attr.Data (Attr (..)) where

import Data.Bifunctor
import Data.Binary
import GHC.Generics (Generic)
import Language.Common.IsConstLike

data Attr name binder = Attr
  { consNameList :: [(name, [binder], IsConstLike)],
    isConstLike :: IsConstLike
  }
  deriving (Show, Generic)

instance (Binary name, Binary binder) => Binary (Attr name binder)

instance Functor (Attr name) where
  fmap = second

instance Bifunctor Attr where
  bimap f g attr =
    attr {consNameList = map (\(n, bs, cl) -> (f n, map g bs, cl)) (consNameList attr)}
