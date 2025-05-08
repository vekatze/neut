module Language.Common.Rule.Attr.VarGlobal (Attr (..), new) where

import Data.Binary
import GHC.Generics (Generic)
import Language.Common.Rule.ArgNum
import Language.Common.Rule.IsConstLike

data Attr = Attr
  { argNum :: ArgNum,
    isConstLike :: IsConstLike
  }
  deriving (Show, Generic)

instance Binary Attr

new :: ArgNum -> Attr
new argNum =
  Attr {argNum = argNum, isConstLike = False}
