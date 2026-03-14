module Language.Term.Inline.Handle
  ( DefMap,
    TypeDefMap,
    Handle (..),
    DefInfo (..),
    DefKind (..),
    GuardEntry (..),
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Gensym.Handle qualified as GensymHandle
import Kernel.Elaborate.Internal.Handle.TypeDef qualified as TypeDef
import Language.Common.Binder
import Language.Common.DefiniteDescription qualified as DD
import Language.Term.Refresh qualified as Refresh
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

data GuardEntry = GuardEntry
  { guardFunction :: DD.DefiniteDescription,
    guardTypeArgs :: [TM.Type],
    guardSelf :: TM.Term
  }

data Handle = Handle
  { substHandle :: Subst.Handle,
    refreshHandle :: Refresh.Handle,
    dmap :: DefMap,
    typeDefMap :: TypeDefMap,
    inlineLimit :: Int,
    currentStepRef :: IORef Int,
    location :: Hint,
    guardStack :: IORef [GuardEntry],
    macroCallStack :: IORef [(DD.DefiniteDescription, Hint)],
    gensymHandle :: GensymHandle.Handle
  }
