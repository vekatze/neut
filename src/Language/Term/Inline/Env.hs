module Language.Term.Inline.Env
  ( Env (..),
  )
where

import Data.IORef
import Data.Text qualified as T
import Gensym.Handle qualified as GensymHandle
import Kernel.Common.Handle.Global.Data qualified as Data
import Language.Common.DataSize qualified as DS
import Language.Term.Inline.Handle
import Language.Term.Stmt qualified as Stmt

data Env = Env
  { gensymHandle :: GensymHandle.Handle,
    dmap :: DefMap,
    typeDefMap :: TypeDefMap,
    dataHandle :: Data.Handle,
    baseSize :: DS.DataSize,
    inlineLimit :: Int,
    specializationTable :: IORef SpecializationTable,
    pendingSpecializationDefs :: IORef [Stmt.Stmt],
    residualCheckList :: IORef [ResidualCheck],
    mainModuleDir :: T.Text
  }
