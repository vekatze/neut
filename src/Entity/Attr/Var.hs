module Entity.Attr.Var (Attr (..)) where

import Data.Binary
import Entity.IsExplicit
import GHC.Generics (Generic)

newtype Attr = Attr
  { isExplicit :: IsExplicit
  }
  deriving (Show, Generic)

instance Binary Attr
