module Entity.WeakTerm where

import Control.Comonad.Cofree
import Data.IntMap qualified as IntMap
import Entity.Annotation qualified as AN
import Entity.Attr.Data qualified as AttrD
import Entity.Attr.DataIntro qualified as AttrDI
import Entity.Attr.VarGlobal qualified as AttrVG
import Entity.Binder
import Entity.DecisionTree qualified as DT
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.HoleID
import Entity.Ident
import Entity.LamKind
import Entity.Magic
import Entity.Noema qualified as N
import Entity.Opacity qualified as O
import Entity.PrimNumSize
import Entity.PrimType qualified as PT
import Entity.Remark
import Entity.WeakPrim qualified as WP

type WeakTerm = Cofree WeakTermF Hint

data WeakTermF a
  = Tau
  | Var Ident
  | VarGlobal AttrVG.Attr DD.DefiniteDescription
  | Pi [BinderF a] a
  | PiIntro (LamKindF a) [BinderF a] a
  | PiElim a [a]
  | Data AttrD.Attr DD.DefiniteDescription [a]
  | DataIntro AttrDI.Attr DD.DefiniteDescription [a] [a] -- (consName, dataArgs, consArgs)
  | DataElim N.IsNoetic [(Ident, a, a)] (DT.DecisionTree a)
  | Noema a
  | Embody a a
  | Let LetOpacity (BinderF a) a a
  | Prim (WP.WeakPrim a)
  | ResourceType DD.DefiniteDescription
  | Magic (Magic a) -- (magic kind arg-1 ... arg-n)
  | Hole HoleID [WeakTerm] -- ?M @ (e1, ..., en)
  | Annotation RemarkLevel (AN.Annotation a) a

type SubstWeakTerm =
  IntMap.IntMap (Either Ident WeakTerm)

data LetOpacity
  = Opaque
  | Transparent
  | Noetic
  deriving (Show, Eq)

reifyOpacity :: LetOpacity -> O.Opacity
reifyOpacity letOpacity =
  case letOpacity of
    Opaque ->
      O.Opaque
    Transparent ->
      O.Transparent
    Noetic ->
      O.Transparent

reflectOpacity :: O.Opacity -> LetOpacity
reflectOpacity opacity =
  case opacity of
    O.Opaque ->
      Opaque
    O.Transparent ->
      Transparent

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
