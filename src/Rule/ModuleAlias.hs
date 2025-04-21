module Rule.ModuleAlias
  ( ModuleAlias (..),
    defaultModuleAlias,
    coreModuleAlias,
    baseModuleAlias,
    reify,
  )
where

import Data.Binary
import Data.Hashable
import Data.Text qualified as T
import Rule.BaseName qualified as BN
import GHC.Generics (Generic)

newtype ModuleAlias = ModuleAlias {extract :: BN.BaseName}
  deriving (Eq, Show, Generic)

instance Hashable ModuleAlias

instance Binary ModuleAlias

defaultModuleAlias :: ModuleAlias
defaultModuleAlias =
  ModuleAlias BN.this

coreModuleAlias :: ModuleAlias
coreModuleAlias =
  ModuleAlias BN.core

baseModuleAlias :: ModuleAlias
baseModuleAlias =
  ModuleAlias BN.base

reify :: ModuleAlias -> T.Text
reify alias =
  BN.reify (extract alias)
