module Language.Common.Rule.Attr.DataIntro (Attr (..)) where

import Data.Bifunctor
import Data.Binary
import GHC.Generics (Generic)
import Language.Common.Rule.Discriminant qualified as D
import Language.Common.Rule.IsConstLike

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
