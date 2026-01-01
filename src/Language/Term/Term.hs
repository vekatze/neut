module Language.Term.Term
  ( Term,
    TermF (..),
    Type,
    TypeF (..),
    TypeEnv,
    insTypeEnv,
    isValue,
    isTypeValue,
    fromLetSeq,
    fromLetSeqOpaque,
    metaOfType,
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
import Language.Common.Binder
import Language.Common.DecisionTree qualified as DT
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Ident
import Language.Common.Ident.Reify
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic
import Language.Common.Noema qualified as N
import Language.Common.Opacity qualified as O
import Language.Common.PiKind (PiKind)
import Language.Common.PrimType qualified as PT
import Language.Common.BaseLowType qualified as BLT
import Language.Term.PrimValue qualified as PV
import Logger.Hint

-- Type representation
type Type = Cofree TypeF Hint

type ID = Int

data TypeF a
  = Tau
  | TVar Ident
  | TVarGlobal AttrVG.Attr DD.DefiniteDescription
  | TyApp a [a]
  | Pi PiKind [BinderF a] [(BinderF a, Term)] [BinderF a] a
  | Data (AttrD.Attr DD.DefiniteDescription (BinderF a)) DD.DefiniteDescription [a]
  | Box a
  | BoxNoema a
  | Code a
  | PrimType PT.PrimType
  | Void
  | Resource DD.DefiniteDescription ID Type Term Term Term
  deriving (Generic)

instance (Binary a) => Binary (TypeF a)

instance Binary Type

instance Binary (Cofree TypeF ())

-- Term representation
type Term = Cofree TermF Hint

data TermF a
  = Var Ident
  | VarGlobal AttrVG.Attr DD.DefiniteDescription
  | PiIntro (AttrL.Attr Type) [BinderF Type] [(BinderF Type, a)] [BinderF Type] a
  | PiElim N.IsNoetic a [Type] [a]
  | DataIntro (AttrDI.Attr DD.DefiniteDescription (BinderF Type)) DD.DefiniteDescription [Type] [a]
  | DataElim N.IsNoetic [(Ident, a, Type)] (DT.DecisionTree Type a)
  | BoxIntro [(BinderF Type, a)] a
  | BoxElim [(BinderF Type, a)] (BinderF Type) a [(BinderF Type, a)] a
  | CodeIntro a
  | CodeElim a
  | Let O.Opacity (BinderF Type) a a
  | Prim (PV.PrimValue Type)
  | Magic (Magic BLT.BaseLowType Type a)
  deriving (Generic)

instance (Binary a) => Binary (TermF a)

instance Binary Term

instance Binary (Cofree TermF ())

type TypeEnv =
  IntMap.IntMap Type

insTypeEnv :: [BinderF Type] -> TypeEnv -> TypeEnv
insTypeEnv xts tenv =
  case xts of
    [] ->
      tenv
    (_, x, t) : rest ->
      insTypeEnv rest $ IntMap.insert (toInt x) t tenv

isTypeValue :: Type -> Bool
isTypeValue ty =
  case ty of
    _ :< Tau ->
      True
    _ :< TVar {} ->
      True
    _ :< TVarGlobal {} ->
      True
    _ :< TyApp t args ->
      isTypeValue t && all isTypeValue args
    _ :< Pi {} ->
      True
    _ :< Data _ _ args ->
      all isTypeValue args
    _ :< Box {} ->
      True
    _ :< BoxNoema {} ->
      True
    _ :< Code {} ->
      True
    _ :< PrimType {} ->
      True
    _ :< Void ->
      True
    _ :< Resource {} ->
      True

isValue :: Term -> Bool
isValue term =
  case term of
    _ :< Var {} ->
      True
    _ :< VarGlobal {} ->
      True
    _ :< PiIntro {} ->
      True
    _ :< DataIntro _ _ dataArgs consArgs ->
      all isTypeValue dataArgs && all isValue consArgs
    _ :< CodeIntro {} ->
      True
    _ :< Prim {} ->
      True
    _ :< Magic (LowMagic (LM.OpaqueValue _)) ->
      True
    _ ->
      False

metaOfType :: Type -> Hint
metaOfType (m :< _) =
  m

fromLetSeq :: [(BinderF Type, Term)] -> Term -> Term
fromLetSeq xts cont =
  case xts of
    [] ->
      cont
    (mxt@(m, _, _), e) : rest ->
      m :< Let O.Clear mxt e (fromLetSeq rest cont)

fromLetSeqOpaque :: [(BinderF Type, Term)] -> Term -> Term
fromLetSeqOpaque xts cont =
  case xts of
    [] ->
      cont
    (mxt@(m, _, _), e) : rest ->
      m :< Let O.Opaque mxt e (fromLetSeq rest cont)
