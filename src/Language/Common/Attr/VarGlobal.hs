module Language.Common.Attr.VarGlobal (Attr (..), new) where

import Data.Binary
import GHC.Generics (Generic)
import Language.Common.ArgNum
import Language.Common.IsConstLike
import Language.Common.IsScript

data Attr = Attr
  { argNum :: ArgNum,
    isConstLike :: IsConstLike,
    isScript :: IsScript
  }
  deriving (Show, Generic)

instance Binary Attr

new :: ArgNum -> Attr
new argNum =
  Attr {argNum = argNum, isConstLike = False, isScript = False}
