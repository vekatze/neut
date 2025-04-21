module Rule.Attr.Data (Attr (..)) where

import Data.Bifunctor
import Data.Binary
import Rule.IsConstLike
import GHC.Generics (Generic)

data Attr name = Attr
  { consNameList :: [(name, IsConstLike)],
    isConstLike :: IsConstLike
  }
  deriving (Show, Generic)

instance (Binary name) => Binary (Attr name)

instance Functor Attr where
  fmap f attr =
    attr {consNameList = map (first f) (consNameList attr)}
