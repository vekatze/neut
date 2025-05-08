module Language.Common.Rule.Attr.Data (Attr (..)) where

import Data.Bifunctor
import Data.Binary
import GHC.Generics (Generic)
import Language.Common.Rule.IsConstLike

data Attr name = Attr
  { consNameList :: [(name, IsConstLike)],
    isConstLike :: IsConstLike
  }
  deriving (Show, Generic)

instance (Binary name) => Binary (Attr name)

instance Functor Attr where
  fmap f attr =
    attr {consNameList = map (first f) (consNameList attr)}
