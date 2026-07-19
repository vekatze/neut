module Language.Common.ModuleAlias
  ( ModuleAlias (..),
    coreModuleAlias,
    baseModuleAlias,
    thisModuleAlias,
    isPrivate,
    reify,
  )
where

import Data.Binary
import Data.Hashable
import Data.Text qualified as T
import GHC.Generics (Generic)
import Language.Common.BaseName qualified as BN

newtype ModuleAlias = ModuleAlias {extract :: BN.BaseName}
  deriving (Eq, Show, Generic)

instance Hashable ModuleAlias

instance Binary ModuleAlias

coreModuleAlias :: ModuleAlias
coreModuleAlias =
  ModuleAlias BN.core

baseModuleAlias :: ModuleAlias
baseModuleAlias =
  ModuleAlias BN.base

thisModuleAlias :: ModuleAlias
thisModuleAlias =
  ModuleAlias BN.this

reify :: ModuleAlias -> T.Text
reify alias =
  BN.reify (extract alias)

isPrivate :: ModuleAlias -> Bool
isPrivate alias =
  "_" `T.isPrefixOf` reify alias
