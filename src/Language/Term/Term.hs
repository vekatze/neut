module Language.Term.Term
  ( Term,
    TermF (..),
    TypeEnv,
    insTypeEnv,
    isValue,
    fromLetSeq,
    fromLetSeqOpaque,
  )
where

import Control.Comonad.Cofree
import Data.Binary
import Data.IntMap qualified as IntMap
import GHC.Generics (Generic)
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Attr.VarGlobal qualified as AttrVG
import Language.Common.BaseLowType
import Language.Common.Binder
import Language.Common.DecisionTree qualified as DT
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Ident
import Language.Common.Ident.Reify
import Language.Common.Magic
import Language.Common.Noema qualified as N
import Language.Common.Opacity qualified as O
import Language.Common.PiKind (PiKind)
import Language.Term.Prim qualified as P
import Logger.Hint

type Term = Cofree TermF Hint

type ID = Int

data TermF a
  = Tau
  | Var Ident
  | VarGlobal AttrVG.Attr DD.DefiniteDescription
  | Pi PiKind [(BinderF a, Maybe a)] [BinderF a] a
  | PiIntro (AttrL.Attr a) [(BinderF a, Maybe a)] [BinderF a] a
  | PiElim N.IsNoetic a [a] [a]
  | Data (AttrD.Attr DD.DefiniteDescription) DD.DefiniteDescription [a]
  | DataIntro (AttrDI.Attr DD.DefiniteDescription) DD.DefiniteDescription [a] [a] -- (consName, dataArgs, consArgs)
  | DataElim N.IsNoetic [(Ident, a, a)] (DT.DecisionTree a)
  | Box a
  | BoxNoema a
  | BoxIntro [(BinderF a, a)] a
  | BoxElim [(BinderF a, a)] (BinderF a) a [(BinderF a, a)] a
  | Let O.Opacity (BinderF a) a a
  | Prim (P.Prim a)
  | Magic (Magic BaseLowType a)
  | Resource DD.DefiniteDescription ID a a a a
  | Void
  deriving (Show, Generic)

instance (Binary a) => Binary (TermF a)

instance Binary Term

instance Binary (Cofree TermF ())

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
    _ :< Var {} ->
      True -- CBV
    _ :< VarGlobal {} ->
      True
    _ :< Pi {} ->
      True
    _ :< PiIntro {} ->
      True
    _ :< Data _ _ args ->
      all isValue args
    _ :< DataIntro _ _ dataArgs consArgs ->
      all isValue $ dataArgs ++ consArgs
    _ :< Box {} ->
      True
    _ :< BoxNoema {} ->
      True
    _ :< Prim {} ->
      True
    _ :< Resource {} ->
      True
    _ :< Void ->
      True
    _ :< Magic (OpaqueValue _) ->
      True
    _ ->
      False

fromLetSeq :: [(BinderF Term, Term)] -> Term -> Term
fromLetSeq xts cont =
  case xts of
    [] ->
      cont
    (mxt@(m, _, _), e) : rest ->
      m :< Let O.Clear mxt e (fromLetSeq rest cont)

fromLetSeqOpaque :: [(BinderF Term, Term)] -> Term -> Term
fromLetSeqOpaque xts cont =
  case xts of
    [] ->
      cont
    (mxt@(m, _, _), e) : rest ->
      m :< Let O.Opaque mxt e (fromLetSeq rest cont)
