module Entity.Attr.VarGlobal (Attr (..), new) where

import Data.Binary
import Entity.ArgNum
import Entity.IsConstLike
import GHC.Generics (Generic)

data Attr = Attr
  { argNum :: ArgNum,
    isConstLike :: IsConstLike
  }
  deriving (Show, Generic)

instance Binary Attr

new :: ArgNum -> Attr
new argNum =
  Attr {argNum = argNum, isConstLike = False}
