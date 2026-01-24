module Language.Term.Term
  ( Term,
    TermF (..),
    Type,
    TypeF (..),
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
import Language.Common.BaseLowType qualified as BLT
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
  | Pi PiKind [BinderF a] [BinderF a] [BinderF a] a
  | Data (AttrD.Attr DD.DefiniteDescription (BinderF a)) DD.DefiniteDescription [a]
  | Box a
  | BoxNoema a
  | Code a
  | PrimType PT.PrimType
  | Void
  | Resource DD.DefiniteDescription ID
  deriving (Generic)

instance (Binary a) => Binary (TypeF a)

instance Binary Type

instance Binary (Cofree TypeF ())

-- Term representation
type Term = Cofree TermF Hint

data TermF a
  = Var Ident
  | VarGlobal AttrVG.Attr DD.DefiniteDescription
  | PiIntro (AttrL.Attr Type) [BinderF Type] [BinderF Type] [(BinderF Type, a)] a
  | PiElim N.IsNoetic a [Type] [a] [Maybe a]
  | DataIntro (AttrDI.Attr DD.DefiniteDescription (BinderF Type)) DD.DefiniteDescription [Type] [a]
  | DataElim N.IsNoetic [(Ident, a, Type)] (DT.DecisionTree Type a)
  | BoxIntro [(BinderF Type, a)] a
  | BoxElim [(BinderF Type, a)] (BinderF Type) a [(BinderF Type, a)] a
  | CodeIntro a
  | CodeElim a
  | TauIntro Type
  | TauElim (Hint, Ident) a a
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

isValue :: Term -> Bool
isValue term =
  case term of
    _ :< Var {} ->
      True
    _ :< VarGlobal {} ->
      True
    _ :< PiIntro {} ->
      True
    _ :< DataIntro _ _ _ consArgs ->
      all isValue consArgs
    _ :< CodeIntro {} ->
      True
    _ :< TauIntro _ ->
      True
    _ :< Prim {} ->
      True
    _ :< Magic (LowMagic (LM.OpaqueValue _)) ->
      True
    _ ->
      False

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
