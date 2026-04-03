module Language.Common.Attr.Data (Attr (..)) where

import Data.Binary
import GHC.Generics (Generic)
import Language.Common.IsConstLike

newtype Attr = Attr
  { isConstLike :: IsConstLike
  }
  deriving (Show, Eq, Generic)

instance Binary Attr
