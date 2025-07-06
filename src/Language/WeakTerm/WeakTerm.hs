module Language.WeakTerm.WeakTerm
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
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Foreign
import Language.Common.HoleID
import Language.Common.Ident
import Language.Common.ImpArgs qualified as ImpArgs
import Language.Common.Magic
import Language.Common.Noema qualified as N
import Language.Common.Opacity qualified as O
import Language.Common.PiKind (PiKind)
import Language.Common.PrimNumSize
import Language.Common.PrimType qualified as PT
import Language.WeakTerm.WeakPrim qualified as WP
import Logger.Hint
import Logger.LogLevel

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
