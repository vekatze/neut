module Entity.Term where

import Control.Comonad.Cofree
import Data.Binary
import qualified Data.IntMap as IntMap
import Entity.Arity
import Entity.Binder
import qualified Entity.DecisionTree as DT
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Discriminant as D
import Entity.Hint
import Entity.Ident
import Entity.Ident.Reify
import Entity.LamKind
import Entity.Magic
import qualified Entity.Prim as P
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
  | DataElim [(Ident, a, a)] (DT.DecisionTree a)
  | Sigma [BinderF a]
  | SigmaIntro [a]
  | SigmaElim [BinderF a] a a
  | Let (BinderF a) a a -- これ、もう不要かも。
  | Prim P.Prim
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
    _ :< Sigma {} ->
      True
    _ :< SigmaIntro es ->
      all isValue es
    _ ->
      False
