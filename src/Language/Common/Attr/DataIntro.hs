module Language.Common.Attr.DataIntro (Attr (..)) where

import Data.Bifunctor
import Data.Binary
import GHC.Generics (Generic)
import Language.Common.Discriminant qualified as D
import Language.Common.IsConstLike

data Attr name binder = Attr
  { dataName :: name,
    consNameList :: [(name, [binder], IsConstLike)],
    discriminant :: D.Discriminant,
    isConstLike :: IsConstLike
  }
  deriving (Show, Generic)

instance (Binary name, Binary binder) => Binary (Attr name binder)

instance Functor (Attr name) where
  fmap = second

instance Bifunctor Attr where
  bimap f g attr =
    attr
      { dataName = f (dataName attr),
        consNameList = map (\(n, bs, cl) -> (f n, map g bs, cl)) (consNameList attr)
      }
