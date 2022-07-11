module Entity.ModuleAlias where

import Data.Binary
import Data.Hashable
import qualified Data.Text as T
import GHC.Generics (Generic)

newtype ModuleAlias = ModuleAlias {extract :: T.Text}
  deriving (Eq, Show, Generic)

instance Hashable ModuleAlias

instance Binary ModuleAlias
