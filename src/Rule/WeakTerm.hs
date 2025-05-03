module Rule.WeakTerm
  ( WeakTerm,
    WeakTermF (..),
    SubstWeakTerm,
    LetOpacity (..),
    reifyOpacity,
    reflectOpacity,
    intTypeBySize,
    metaOf,
    piElim,
    fromLetSeq,
    fromBaseLowType,
  )
where

import Control.Comonad.Cofree
import Data.IntMap qualified as IntMap
import Logger.Rule.LogLevel
import Rule.Annotation qualified as AN
import Rule.Attr.Data qualified as AttrD
import Rule.Attr.DataIntro qualified as AttrDI
import Rule.Attr.Lam qualified as AttrL
import Rule.Attr.VarGlobal qualified as AttrVG
import Rule.BaseLowType qualified as BLT
import Rule.BasePrimType qualified as BPT
import Rule.Binder
import Rule.DecisionTree qualified as DT
import Rule.DefiniteDescription qualified as DD
import Rule.Hint
import Rule.HoleID
import Rule.Ident
import Rule.Magic
import Rule.Noema qualified as N
import Rule.Opacity qualified as O
import Rule.PrimNumSize
import Rule.PrimType qualified as PT
import Rule.WeakPrim qualified as WP

type WeakTerm = Cofree WeakTermF Hint

data WeakTermF a
  = Tau
  | Var Ident
  | VarGlobal AttrVG.Attr DD.DefiniteDescription
  | Pi [BinderF a] [BinderF a] a
  | PiIntro (AttrL.Attr a) [BinderF a] [BinderF a] a
  | PiElim a [a]
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
  | Resource DD.DefiniteDescription Int a a a
  | Use a [BinderF a] a
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

intTypeBySize :: Hint -> Int -> WeakTerm
intTypeBySize m size =
  m :< Prim (WP.Type $ PT.Int $ IntSize size)

metaOf :: WeakTerm -> Hint
metaOf (m :< _) =
  m

piElim :: a -> [a] -> WeakTermF a
piElim =
  PiElim

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
