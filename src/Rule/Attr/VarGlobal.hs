module Rule.Attr.VarGlobal (Attr (..), new) where

import Data.Binary
import Rule.ArgNum
import Rule.IsConstLike
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
