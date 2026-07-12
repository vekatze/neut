module Language.Term.Inline.Env
  ( Env (..),
  )
where

import Data.IORef
import Gensym.Handle qualified as GensymHandle
import Kernel.Common.Handle.Global.Data qualified as Data
import Kernel.Common.Handle.Global.ModulePath qualified as ModulePath
import Kernel.Common.Handle.Local.Tag qualified as Tag
import Kernel.Common.Module qualified as Module
import Language.Common.DataSize qualified as DS
import Language.Term.Inline.Handle
import Language.Term.Stmt qualified as Stmt

data Env = Env
  { gensymHandle :: GensymHandle.Handle,
    dmap :: DefMap,
    typeDefMap :: TypeDefMap,
    tropeMap :: TropeMap,
    tagHandle :: Tag.Handle,
    dataHandle :: Data.Handle,
    baseSize :: DS.DataSize,
    inlineLimit :: Int,
    specializationTable :: IORef SpecializationTable,
    pendingSpecializationDefs :: IORef [Stmt.Stmt],
    residualCheckList :: IORef [ResidualCheck],
    mainModule :: Module.MainModule,
    modulePathMap :: ModulePath.ModulePathMap
  }
