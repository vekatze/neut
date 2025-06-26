module Language.WeakTerm.Rule.WeakTerm
  ( WeakTerm,
    WeakTermF (..),
    SubstWeakTerm,
    LetOpacity (..),
    WeakForeign,
    reifyOpacity,
    reflectOpacity,
    intTypeBySize,
    metaOf,
    fromLetSeq,
    fromBaseLowType,
  )
where

import Control.Comonad.Cofree
import Data.IntMap qualified as IntMap
import Language.Common.Rule.Annotation qualified as AN
import Language.Common.Rule.Attr.Data qualified as AttrD
import Language.Common.Rule.Attr.DataIntro qualified as AttrDI
import Language.Common.Rule.Attr.Lam qualified as AttrL
import Language.Common.Rule.Attr.VarGlobal qualified as AttrVG
import Language.Common.Rule.BaseLowType qualified as BLT
import Language.Common.Rule.BasePrimType qualified as BPT
import Language.Common.Rule.Binder
import Language.Common.Rule.DataSize (DataSize)
import Language.Common.Rule.DecisionTree qualified as DT
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Foreign
import Language.Common.Rule.HoleID
import Language.Common.Rule.Ident
import Language.Common.Rule.ImpArgs qualified as ImpArgs
import Language.Common.Rule.Magic
import Language.Common.Rule.Noema qualified as N
import Language.Common.Rule.Opacity qualified as O
import Language.Common.Rule.PiKind (PiKind)
import Language.Common.Rule.PrimNumSize
import Language.Common.Rule.PrimType qualified as PT
import Language.WeakTerm.Rule.WeakPrim qualified as WP
import Logger.Rule.Hint
import Logger.Rule.LogLevel

type WeakTerm = Cofree WeakTermF Hint

data WeakTermF a
  = Tau
  | Var Ident
  | VarGlobal AttrVG.Attr DD.DefiniteDescription
  | Pi PiKind [(BinderF a, Maybe a)] [BinderF a] a
  | PiIntro (AttrL.Attr a) [(BinderF a, Maybe a)] [BinderF a] a
  | PiElim N.IsNoetic a (ImpArgs.ImpArgs a) [a]
  | PiElimExact a
  | Data (AttrD.Attr DD.DefiniteDescription) DD.DefiniteDescription [a]
  | DataIntro (AttrDI.Attr DD.DefiniteDescription) DD.DefiniteDescription [a] [a] -- (consName, dataArgs, consArgs)
  | DataElim N.IsNoetic [(Ident, a, a)] (DT.DecisionTree a)
  | Box a
  | BoxNoema a
  | BoxIntro [(BinderF a, a)] a
  | BoxIntroQuote a
  | BoxElim [(BinderF a, a)] (BinderF a) a [(BinderF a, a)] a
  | Actual a
  | Let LetOpacity (BinderF a) a a
  | Prim (WP.WeakPrim a)
  | Magic (WeakMagic a) -- (magic kind arg-1 ... arg-n)
  | Hole HoleID [WeakTerm] -- ?M @ (e1, ..., en)
  | Annotation LogLevel (AN.Annotation a) a
  | Resource DD.DefiniteDescription Int a a a a
  | Void

type SubstWeakTerm =
  IntMap.IntMap (Either Ident WeakTerm)

data LetOpacity
  = Opaque
  | Clear
  | Noetic
  deriving (Show, Eq)

reifyOpacity :: LetOpacity -> O.Opacity
reifyOpacity letOpacity =
  case letOpacity of
    Opaque ->
      O.Opaque
    Clear ->
      O.Clear
    Noetic ->
      O.Clear

reflectOpacity :: O.Opacity -> LetOpacity
reflectOpacity opacity =
  case opacity of
    O.Opaque ->
      Opaque
    O.Clear ->
      Clear

intTypeBySize :: Hint -> DataSize -> WeakTerm
intTypeBySize m size =
  m :< Prim (WP.Type $ PT.Int $ dataSizeToIntSize size)

metaOf :: WeakTerm -> Hint
metaOf (m :< _) =
  m

fromLetSeq :: [(BinderF WeakTerm, WeakTerm)] -> WeakTerm -> WeakTerm
fromLetSeq xts cont =
  case xts of
    [] ->
      cont
    (mxt@(m, _, _), e) : rest ->
      m :< Let Clear mxt e (fromLetSeq rest cont)

fromBaseLowType :: Hint -> BLT.BaseLowType -> WeakTerm
fromBaseLowType m lt =
  case lt of
    BLT.PrimNum pt ->
      case pt of
        BPT.Int s ->
          m :< Prim (WP.Type (PT.Int (BPT.extractSize s)))
        BPT.Float s ->
          m :< Prim (WP.Type (PT.Float (BPT.extractSize s)))
    BLT.Pointer ->
      m :< Prim (WP.Type PT.Pointer)

type WeakForeign =
  BaseForeign WeakTerm
