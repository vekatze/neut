module Entity.Attr.DataIntro (Attr (..)) where

import Data.Bifunctor
import Data.Binary
import Entity.Discriminant qualified as D
import Entity.IsConstLike
import GHC.Generics (Generic)

data Attr name = Attr
  { dataName :: name,
    consNameList :: [(name, IsConstLike)],
    discriminant :: D.Discriminant,
    isConstLike :: IsConstLike
  }
  deriving (Show, Generic)

instance (Binary name) => Binary (Attr name)

instance Functor Attr where
  fmap f attr =
    attr
      { dataName = f (dataName attr),
        consNameList = map (first f) (consNameList attr)
      }
