module Entity.WeakTerm where

import Control.Comonad.Cofree
import Data.IntMap qualified as IntMap
import Entity.Annotation qualified as AN
import Entity.Attr.Data qualified as AttrD
import Entity.Attr.DataIntro qualified as AttrDI
import Entity.Attr.Lam qualified as AttrL
import Entity.Attr.VarGlobal qualified as AttrVG
import Entity.Binder
import Entity.DecisionTree qualified as DT
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.HoleID
import Entity.Ident
import Entity.Magic
import Entity.Noema qualified as N
import Entity.Opacity qualified as O
import Entity.PrimNumSize
import Entity.PrimType qualified as PT
import Entity.Remark
import Entity.WeakPrim qualified as WP

type WeakTerm = Cofree WeakTermF Hint

type ID = Int

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
  | BoxIntro a
  | Noema a
  | Embody a a
  | Actual a
  | Let LetOpacity (BinderF a) a a
  | Prim (WP.WeakPrim a)
  | Magic (Magic a) -- (magic kind arg-1 ... arg-n)
  | Hole HoleID [WeakTerm] -- ?M @ (e1, ..., en)
  | Annotation RemarkLevel (AN.Annotation a) a
  | Resource Int a a
  | Use a [BinderF a] a

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

toVar :: Hint -> Ident -> WeakTerm
toVar m x =
  m :< Var x

intTypeBySize :: Hint -> Int -> WeakTerm
intTypeBySize m size =
  m :< Prim (WP.Type $ PT.Int $ IntSize size)

metaOf :: WeakTerm -> Hint
metaOf (m :< _) =
  m

asVar :: WeakTerm -> Maybe Ident
asVar term =
  case term of
    (_ :< Var x) ->
      Just x
    _ ->
      Nothing

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
