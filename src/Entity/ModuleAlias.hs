module Entity.ModuleAlias where

import qualified Data.Text as T

newtype ModuleAlias = ModuleAlias {extract :: T.Text}
  deriving (Eq)
