module Language.Term.Inline.Handle
  ( DefMap,
    TypeDefMap,
    SpecializationTable,
    Handle (..),
    DefInfo (..),
    DefKind (..),
    SpecializationEntry (..),
    TropeMap,
    Normal (..),
    TropeDepth,
    Pending (..),
    PendingTerm (..),
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.IORef
import Gensym.Handle qualified as GensymHandle
import Kernel.Common.Handle.Global.Data qualified as Data
import Kernel.Common.Handle.Global.OptimizableData qualified as OptimizableData
import Kernel.Common.Handle.Local.Tag qualified as Tag
import Kernel.Common.Module qualified as Module
import Kernel.Common.Handle.Global.ModulePath qualified as ModulePath
import Kernel.Elaborate.Internal.Handle.TypeDef qualified as TypeDef
import Language.Common.Binder
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Ident (Ident)
import Language.Term.Stmt qualified as Stmt
import Language.Term.Subst qualified as Subst
import Language.Term.Term qualified as TM
import Language.Term.Trace qualified as Trace
import Logger.Hint

data Normal = Normal
  { normalTerm :: TM.Term,
    isNormalValue :: Bool
  }

type TropeDepth =
  Int

data Pending = Pending
  { pendingSerial :: Int,
    pendingTerm :: PendingTerm
  }

data PendingTerm
  = Normalized TropeDepth Normal
  | Renamed Ident
  | TypeValue TM.Type

data DefKind
  = NoInline
  | Inline
  | Macro
  | MacroInline
  | ConstantMeta
  | DataIntro
  deriving (Eq, Show)

data DefInfo = DefInfo
  { defImpBinders :: [BinderF TM.Type],
    defExpBinders :: [BinderF TM.Type],
    defDefaultArgs :: [(BinderF TM.Type, TM.Term)],
    defBody :: TM.Term,
    codType :: TM.Type,
    defKind :: DefKind,
    traceSiteIDs :: IntSet.IntSet
  }

type DefMap =
  Map.HashMap DD.DefiniteDescription DefInfo

type TypeDefMap =
  TypeDef.TypeDefMap

type TropeMap =
  Map.HashMap DD.DefiniteDescription [Stmt.DefineMeta]

data SpecializationEntry = SpecializationEntry
  { specializationTypeArgs :: [TM.Type],
    specializationName :: DD.DefiniteDescription
  }

type SpecializationTable =
  Map.HashMap DD.DefiniteDescription [SpecializationEntry]

data Handle = Handle
  { substHandle :: Subst.Handle,
    dmap :: DefMap,
    localMetaDefMap :: IORef (Map.HashMap DD.DefiniteDescription DefInfo),
    typeDefMap :: TypeDefMap,
    tropeMap :: TropeMap,
    tagHandle :: Tag.Handle,
    dataHandle :: Data.Handle,
    optDataHandle :: OptimizableData.Handle,
    inlineLimit :: Int,
    currentStepRef :: IORef Int,
    location :: Hint,
    specializationTable :: IORef SpecializationTable,
    pendingSpecializationDefs :: IORef [Stmt.Stmt],
    traceEnabled :: Bool,
    macroCallStack :: IORef [(DD.DefiniteDescription, DefKind, Hint)],
    gensymHandle :: GensymHandle.Handle,
    currentStage :: Int,
    initialStage :: Int,
    insideDefineMeta :: Bool,
    localMetaMemo :: [(DD.DefiniteDescription, [TM.Type], Ident)],
    activeDefineMetaList :: [Stmt.DefineMeta],
    pendingValues :: IntMap.IntMap Pending,
    nextPendingSerial :: Int,
    usedPendingSerialRef :: IORef Int,
    emittedBinders :: IORef IntSet.IntSet,
    mainModule :: Module.MainModule,
    modulePathMap :: ModulePath.ModulePathMap,
    traceHandle :: Trace.Handle,
    localTypeEnv :: TM.TypeEnv
  }
