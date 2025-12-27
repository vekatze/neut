module Language.Common.Attr.DataIntro (Attr (..)) where

import Data.Binary
import GHC.Generics (Generic)
import Language.Common.Discriminant qualified as D
import Language.Common.IsConstLike

data Attr name = Attr
  { dataName :: name,
    consNameList :: [(name, IsConstLike)],
    discriminant :: D.Discriminant,
    isConstLike :: IsConstLike
  }
  deriving (Show, Generic)

instance (Binary name) => Binary (Attr name)
