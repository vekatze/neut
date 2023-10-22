module Entity.Attr.DataIntro where

import Data.Binary
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.IsConstLike
import GHC.Generics (Generic)

data Attr = Attr
  { dataName :: DD.DefiniteDescription,
    consNameList :: [DD.DefiniteDescription],
    discriminant :: D.Discriminant,
    isConstLike :: IsConstLike
  }
  deriving (Show, Generic)

instance Binary Attr
