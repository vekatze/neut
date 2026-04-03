module Language.Common.Attr.DataIntro (Attr (..)) where

import Data.Binary
import GHC.Generics (Generic)
import Language.Common.Discriminant qualified as D
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.IsConstLike

data Attr = Attr
  { dataName :: DD.DefiniteDescription,
    discriminant :: D.Discriminant,
    isConstLike :: IsConstLike
  }
  deriving (Show, Eq, Generic)

instance Binary Attr
