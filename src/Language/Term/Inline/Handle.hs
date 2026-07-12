module Language.Term.Inline.Handle
  ( DefMap,
    TypeDefMap,
    SpecializationTable,
    Handle (..),
    DefInfo (..),
    DefKind (..),
    SpecializationEntry (..),
    TropeMap,
    ResidualCheck (..),
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Gensym.Handle qualified as GensymHandle
import Kernel.Common.Handle.Global.Data qualified as Data
import Kernel.Common.Handle.Local.Tag qualified as Tag
import Kernel.Common.Module qualified as Module
import Kernel.Common.Handle.Global.ModulePath qualified as ModulePath
import Kernel.Elaborate.Internal.Handle.TypeDef qualified as TypeDef
import Language.Common.Binder
import Language.Common.DataSize qualified as DS
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Ident (Ident)
import Language.Term.Stmt qualified as Stmt
import Language.Term.Subst qualified as Subst
import Language.Term.Term qualified as TM
import Logger.Hint

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
    defKind :: DefKind
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

data ResidualCheck
  = CheckActuality Hint TM.Type
  | CheckInteger Hint TM.Type
  | CheckMixable Hint TM.Type

data Handle = Handle
  { substHandle :: Subst.Handle,
    dmap :: DefMap,
    localMetaDefMap :: IORef (Map.HashMap DD.DefiniteDescription DefInfo),
    typeDefMap :: TypeDefMap,
    tropeMap :: TropeMap,
    tagHandle :: Tag.Handle,
    dataHandle :: Data.Handle,
    inlineLimit :: Int,
    currentStepRef :: IORef Int,
    location :: Hint,
    specializationTable :: IORef SpecializationTable,
    pendingSpecializationDefs :: IORef [Stmt.Stmt],
    residualCheckList :: IORef [ResidualCheck],
    shouldEmitResidualChecks :: Bool,
    macroCallStack :: IORef [(DD.DefiniteDescription, DefKind, Hint)],
    gensymHandle :: GensymHandle.Handle,
    baseSize :: DS.DataSize,
    currentStage :: Int,
    initialStage :: Int,
    insideDefineMeta :: Bool,
    localMetaMemo :: [(DD.DefiniteDescription, [TM.Type], Ident)],
    activeDefineMetaList :: [Stmt.DefineMeta],
    mainModule :: Module.MainModule,
    modulePathMap :: ModulePath.ModulePathMap
  }
