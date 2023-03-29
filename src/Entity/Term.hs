module Entity.Term where

import Control.Comonad.Cofree
import Data.Binary
import Data.IntMap qualified as IntMap
import Entity.Arity
import Entity.Binder
import Entity.DecisionTree qualified as DT
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.Hint
import Entity.Ident
import Entity.Ident.Reify
import Entity.LamKind
import Entity.LamKind qualified as LK
import Entity.Magic
import Entity.Noema qualified as N
import Entity.Prim qualified as P
import GHC.Generics (Generic)

type Term = Cofree TermF Hint

data TermF a
  = Tau
  | Var Ident
  | VarGlobal DD.DefiniteDescription Arity
  | Pi [BinderF a] a
  | PiIntro (LamKindF a) [BinderF a] a
  | PiElim a [a]
  | Data DD.DefiniteDescription [a]
  | DataIntro DD.DefiniteDescription DD.DefiniteDescription D.Discriminant [a] [a]
  | DataElim N.IsNoetic [(Ident, a, a)] (DT.DecisionTree a)
  | Noema a
  | Cell a
  | CellIntro a
  | CellElim a
  | Prim P.Prim
  | ResourceType DD.DefiniteDescription
  | Magic (Magic a)
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
    _ :< Pi {} ->
      True
    _ :< PiIntro {} ->
      True
    _ :< Data {} ->
      True
    _ :< DataIntro _ _ _ dataArgs consArgs ->
      all isValue $ dataArgs ++ consArgs
    _ ->
      False

containsNoema :: Term -> Bool
containsNoema term =
  case term of
    _ :< Tau ->
      False
    _ :< Var {} ->
      False
    _ :< VarGlobal {} ->
      False
    _ :< Pi xts cod -> do
      let ts = map (\(_, _, t) -> t) xts
      any containsNoema $ cod : ts
    _ :< PiIntro kind xts e -> do
      let ts = map (\(_, _, t) -> t) xts
      case kind of
        LK.Normal _ ->
          any containsNoema $ e : ts
        LK.Fix (_, _, t) ->
          any containsNoema $ e : t : ts
    _ :< PiElim e es ->
      any containsNoema $ e : es
    _ :< Data _ ts ->
      any containsNoema ts
    _ :< DataIntro _ _ _ dataArgs consArgs ->
      any containsNoema $ dataArgs ++ consArgs
    _ :< DataElim _ oets decisionTree -> do
      let (_, es, ts) = unzip3 oets
      any containsNoema (es ++ ts) || containsNoemaDecisionTree decisionTree
    _ :< Noema {} ->
      True
    _ :< Cell {} ->
      True
    _ :< CellIntro e ->
      containsNoema e
    _ :< CellElim e ->
      containsNoema e
    _ :< Prim {} ->
      False
    _ :< ResourceType {} ->
      False
    _ :< Magic magic ->
      case magic of
        Cast from to e ->
          any containsNoema [from, to, e]
        Store _ e1 e2 ->
          any containsNoema [e1, e2]
        Load _ e ->
          containsNoema e
        Syscall _ es ->
          any containsNoema es
        External _ es ->
          any containsNoema es

containsNoemaDecisionTree :: DT.DecisionTree Term -> Bool
containsNoemaDecisionTree tree =
  case tree of
    DT.Leaf _ e ->
      containsNoema e
    DT.Unreachable ->
      False
    DT.Switch (_, cursor) caseList ->
      containsNoema cursor || containsNoemaCaseList caseList

containsNoemaCaseList :: DT.CaseList Term -> Bool
containsNoemaCaseList (fallbackClause, clauseList) = do
  let xs1 = containsNoemaDecisionTree fallbackClause
  let xs2 = any containsNoemaCase clauseList
  xs1 || xs2

containsNoemaCase :: DT.Case Term -> Bool
containsNoemaCase (DT.Cons _ _ dataArgs consArgs tree) = do
  let (dataTerms, dataTypes) = unzip dataArgs
  let b1 = any containsNoema $ dataTerms ++ dataTypes
  let b2 = any (containsNoema . (\(_, _, t) -> t)) consArgs
  let b3 = containsNoemaDecisionTree tree
  b1 || b2 || b3

containsPi :: Term -> Bool
containsPi term =
  case term of
    _ :< Tau ->
      False
    _ :< Var {} ->
      False
    _ :< VarGlobal {} ->
      False
    _ :< Pi {} -> do
      True
    _ :< PiIntro kind xts e -> do
      let ts = map (\(_, _, t) -> t) xts
      case kind of
        LK.Normal _ ->
          any containsPi $ e : ts
        LK.Fix (_, _, t) ->
          any containsPi $ e : t : ts
    _ :< PiElim e es ->
      any containsPi $ e : es
    _ :< Data _ ts ->
      any containsPi ts
    _ :< DataIntro _ _ _ dataArgs consArgs ->
      any containsPi $ dataArgs ++ consArgs
    _ :< DataElim _ oets decisionTree -> do
      let (_, es, ts) = unzip3 oets
      any containsPi (es ++ ts) || containsPiDecisionTree decisionTree
    _ :< Noema t ->
      containsPi t
    _ :< Cell t ->
      containsPi t
    _ :< CellIntro e ->
      containsPi e
    _ :< CellElim e ->
      containsPi e
    _ :< Prim {} ->
      False
    _ :< ResourceType {} ->
      False
    _ :< Magic magic ->
      case magic of
        Cast from to e ->
          any containsPi [from, to, e]
        Store _ e1 e2 ->
          any containsPi [e1, e2]
        Load _ e ->
          containsPi e
        Syscall _ es ->
          any containsPi es
        External _ es ->
          any containsPi es

containsPiDecisionTree :: DT.DecisionTree Term -> Bool
containsPiDecisionTree tree =
  case tree of
    DT.Leaf _ e ->
      containsPi e
    DT.Unreachable ->
      False
    DT.Switch (_, cursor) caseList ->
      containsPi cursor || containsPiCaseList caseList

containsPiCaseList :: DT.CaseList Term -> Bool
containsPiCaseList (fallbackClause, clauseList) = do
  let xs1 = containsPiDecisionTree fallbackClause
  let xs2 = any containsPiCase clauseList
  xs1 || xs2

containsPiCase :: DT.Case Term -> Bool
containsPiCase (DT.Cons _ _ dataArgs consArgs tree) = do
  let (dataTerms, dataTypes) = unzip dataArgs
  let b1 = any containsPi $ dataTerms ++ dataTypes
  let b2 = any (containsPi . (\(_, _, t) -> t)) consArgs
  let b3 = containsPiDecisionTree tree
  b1 || b2 || b3
