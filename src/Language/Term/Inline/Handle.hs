module Language.Term.Inline.Handle
  ( DefMap,
    TypeDefMap,
    SpecializationTable,
    Handle (..),
    DefInfo (..),
    DefKind (..),
    SpecializationEntry (..),
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Gensym.Handle qualified as GensymHandle
import Kernel.Elaborate.Internal.Handle.TypeDef qualified as TypeDef
import Language.Common.Binder
import Language.Common.DefiniteDescription qualified as DD
import Language.Term.Refresh qualified as Refresh
import Language.Term.Stmt qualified as Stmt
import Language.Term.Subst qualified as Subst
import Language.Term.Term qualified as TM
import Logger.Hint

data DefKind
  = NoInline
  | Inline
  | Macro
  | MacroInline
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

data SpecializationEntry = SpecializationEntry
  { specializationTypeArgs :: [TM.Type],
    specializationName :: DD.DefiniteDescription
  }

type SpecializationTable =
  Map.HashMap DD.DefiniteDescription [SpecializationEntry]

data Handle = Handle
  { substHandle :: Subst.Handle,
    refreshHandle :: Refresh.Handle,
    dmap :: DefMap,
    typeDefMap :: TypeDefMap,
    inlineLimit :: Int,
    currentStepRef :: IORef Int,
    location :: Hint,
    specializationTable :: IORef SpecializationTable,
    pendingSpecializationDefs :: IORef [Stmt.Stmt],
    macroCallStack :: IORef [(DD.DefiniteDescription, Hint)],
    gensymHandle :: GensymHandle.Handle
  }
