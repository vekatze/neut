module Entity.WeakTerm where

import Control.Comonad.Cofree
import Data.IntMap qualified as IntMap
import Entity.Annotation qualified as AN
import Entity.ArgNum
import Entity.Binder
import Entity.DecisionTree qualified as DT
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
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
  | VarGlobal DD.DefiniteDescription ArgNum
  | Pi [BinderF a] a
  | PiIntro (LamKindF a) [BinderF a] a
  | PiElim a [a]
  | Data DD.DefiniteDescription [DD.DefiniteDescription] [a]
  | DataIntro DD.DefiniteDescription DD.DefiniteDescription [DD.DefiniteDescription] D.Discriminant [a] [a]
  | DataElim N.IsNoetic [(Ident, a, a)] (DT.DecisionTree a)
  | Noema a
  | Embody a a
  | Let LetOpacity (BinderF a) a a
  | Prim (WP.WeakPrim a)
  | ResourceType DD.DefiniteDescription
  | Magic (Magic a) -- (magic kind arg-1 ... arg-n)
  | Hole HoleID [WeakTerm] -- ?M @ (e1, ..., en)
  | Annotation RemarkLevel (AN.Annotation a) a
  | Flow DD.DefiniteDescription a -- ("core.thread.flow-inner", (actual-argument, arg-type))
  | FlowIntro
      DD.DefiniteDescription -- "core.thread.flow-inner"
      DD.DefiniteDescription -- "core.thread.detach"
      (a, a) -- (flow-inner, inner-type)
  | FlowElim
      DD.DefiniteDescription -- "core.thread.flow-inner"
      DD.DefiniteDescription -- "core.thread.attach"
      (a, a) -- (flow, flow-type)
  | Nat
  | NatZero
  | NatSucc Integer a

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
    O.Lucent ->
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
