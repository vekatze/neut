module Entity.Attr.Data where

import Data.Binary
import Entity.DefiniteDescription qualified as DD
import Entity.IsConstLike
import GHC.Generics (Generic)

data Attr = Attr
  { consNameList :: [(DD.DefiniteDescription, IsConstLike)],
    isConstLike :: IsConstLike
  }
  deriving (Show, Generic)

instance Binary Attr
