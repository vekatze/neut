module Entity.Term where

import Control.Comonad.Cofree
import Data.Binary
import Data.IntMap qualified as IntMap
import Entity.ArgNum
import Entity.Binder
import Entity.DecisionTree qualified as DT
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.Hint
import Entity.Ident
import Entity.Ident.Reify
import Entity.LamKind
import Entity.Magic
import Entity.Noema qualified as N
import Entity.Opacity qualified as O
import Entity.Prim qualified as P
import GHC.Generics (Generic)

type Term = Cofree TermF Hint

data TermF a
  = Tau
  | Var Ident
  | VarGlobal DD.DefiniteDescription ArgNum
  | Pi [BinderF a] a
  | PiIntro (LamKindF a) [BinderF a] a
  | PiElim a [a]
  | Data DD.DefiniteDescription [DD.DefiniteDescription] [a]
  | DataIntro DD.DefiniteDescription DD.DefiniteDescription [DD.DefiniteDescription] D.Discriminant [a] [a]
  | DataElim N.IsNoetic [(Ident, a, a)] (DT.DecisionTree a)
  | Noema a
  | Embody a a
  | Let O.Opacity (BinderF a) a a
  | Prim (P.Prim a)
  | ResourceType DD.DefiniteDescription
  | Magic (Magic a)
  | Flow DD.DefiniteDescription a -- ("core.thread.flow-inner", (actual-argument, arg-type))
  | FlowIntro
      DD.DefiniteDescription -- "core.thread.flow-inner"
      DD.DefiniteDescription -- "core.thread.run"
      (a, a) -- (lambda, lam-type)
  | FlowElim
      DD.DefiniteDescription -- "core.thread.flow-inner"
      DD.DefiniteDescription -- "core.thread.wait"
      (a, a) -- (flow, flow-type)
  deriving (Show, Generic)

instance (Binary a) => Binary (TermF a)

instance Binary Term

type TypeEnv =
  IntMap.IntMap Term

insTypeEnv :: [BinderF Term] -> TypeEnv -> TypeEnv
insTypeEnv xts tenv =
  case xts of
    [] ->
      tenv
    (_, x, t) : rest ->
      insTypeEnv rest $ IntMap.insert (toInt x) t tenv

isValue :: Term -> Bool
isValue term =
  case term of
    _ :< Tau ->
      True
    _ :< VarGlobal {} ->
      True
    _ :< Pi {} ->
      True
    _ :< PiIntro {} ->
      True
    _ :< Data {} ->
      True
    _ :< DataIntro _ _ _ _ dataArgs consArgs ->
      all isValue $ dataArgs ++ consArgs
    _ :< Noema {} ->
      True
    _ :< Prim {} ->
      True
    _ :< ResourceType {} ->
      True
    _ :< Flow {} ->
      True
    _ ->
      False
