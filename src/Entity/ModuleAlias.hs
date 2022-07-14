module Entity.ModuleAlias where

import Data.Binary
import Data.Hashable
import qualified Entity.BaseName as BN
import GHC.Generics (Generic)

newtype ModuleAlias = ModuleAlias {extract :: BN.BaseName}
  deriving (Eq, Show, Generic)

instance Hashable ModuleAlias

instance Binary ModuleAlias

defaultModulePrefix :: ModuleAlias
defaultModulePrefix =
  ModuleAlias BN.this

baseModulePrefix :: ModuleAlias
baseModulePrefix =
  ModuleAlias BN.base
