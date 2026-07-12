module Language.WeakTerm.WeakTerm
  ( WeakTerm,
    WeakTermF (..),
    WeakType,
    WeakTypeF (..),
    WeakForeign,
    intTypeBySize,
    metaOfType,
    fromLetSeq,
    fromBaseLowType,
  )
where

import Control.Comonad.Cofree
import Language.Common.Annotation qualified as AN
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.DataIntro qualified as AttrDI
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.Attr.VarGlobal qualified as AttrVG
import Language.Common.BaseLowType qualified as BLT
import Language.Common.BasePrimType qualified as BPT
import Language.Common.Binder
import Language.Common.DataSize (DataSize)
import Language.Common.DecisionTree qualified as DT
import Language.Common.DefaultArgs qualified as DefaultArgs
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Foreign
import Language.Common.HoleID
import Language.Common.Ident
import Language.Common.ImpArgs qualified as ImpArgs
import Language.Common.Magic (WeakMagic (..))
import Language.Common.Noema qualified as N
import Language.Common.PiElimKind qualified as PEK
import Language.Common.PiKind (PiKind)
import Language.Common.PrimNumSize
import Language.Common.PrimType qualified as PT
import Language.WeakTerm.WeakPrimValue qualified as WPV
import Logger.Hint
import Logger.LogLevel

type WeakType = Cofree WeakTypeF Hint

data WeakTypeF a
  = Tau
  | TVar Ident
  | TVarGlobal AttrVG.Attr DD.DefiniteDescription
  | TyApp a [a]
  | Pi PiKind [BinderF a] [BinderF a] [BinderF a] a
  | Data AttrD.Attr DD.DefiniteDescription [a]
  | Box a
  | BoxNoema a
  | Code a
  | PrimType PT.PrimType
  | Void
  | Resource DD.DefiniteDescription Int
  | TypeHole HoleID [WeakType]

type WeakTerm = Cofree WeakTermF Hint

data WeakTermF a
  = Var Ident
  | VarGlobal AttrVG.Attr DD.DefiniteDescription
  | PiIntro (AttrL.Attr WeakType) [BinderF WeakType] [BinderF WeakType] [(BinderF WeakType, WeakTerm)] a
  | PiElim (PEK.PiElimKind WeakType) a (ImpArgs.ImpArgs WeakType) [a] (DefaultArgs.DefaultArgs a)
  | PiElimExact a
  | DataIntro AttrDI.Attr DD.DefiniteDescription [WeakType] [a]
  | DataElim N.IsNoetic [(Ident, a, WeakType)] (DT.DecisionTree WeakType a)
  | BoxIntro [(BinderF WeakType, a)] a
  | BoxIntroLift (Maybe WeakType) a
  | BoxElim [(BinderF WeakType, a)] (BinderF WeakType) a [(BinderF WeakType, a)] a
  | CodeIntro a
  | CodeElim a
  | TauIntro WeakType
  | TauElim (Hint, Ident) a a
  | Let (BinderF WeakType) a a
  | Invoke [DD.DefiniteDescription] a
  | Prim (WPV.WeakPrimValue WeakType)
  | Magic (WeakMagic WeakType WeakType a)
  | Annotation LogLevel (AN.Annotation WeakType) a

intTypeBySize :: Hint -> DataSize -> WeakType
intTypeBySize m size =
  m :< PrimType (PT.Int $ dataSizeToIntSize size)

metaOfType :: WeakType -> Hint
metaOfType (m :< _) =
  m

fromLetSeq :: [(BinderF WeakType, WeakTerm)] -> WeakTerm -> WeakTerm
fromLetSeq xts cont =
  case xts of
    [] ->
      cont
    (mxt@(m, _, _, _), e) : rest ->
      m :< Let mxt e (fromLetSeq rest cont)

fromBaseLowType :: Hint -> BLT.BaseLowType -> WeakType
fromBaseLowType m lt =
  case lt of
    BLT.PrimNum pt ->
      case pt of
        BPT.Int s ->
          m :< PrimType (PT.Int (BPT.extractSize s))
        BPT.Float s ->
          m :< PrimType (PT.Float (BPT.extractSize s))
    BLT.Pointer ->
      m :< PrimType PT.Pointer

type WeakForeign =
  BaseForeign WeakType
