module Kernel.Elaborate.Rule.Stuck
  ( EvalBase (..),
    EvalCtx,
    EvalCtxF (..),
    Stuck,
    asStuckedTerm,
    resume,
    asPairList,
  )
where

import Aux.Logger.Rule.Hint
import Control.Comonad.Cofree
import Kernel.Elaborate.Rule.Constraint qualified as C
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.HoleID qualified as HID
import Language.Common.Rule.Ident
import Language.WeakTerm.Rule.WeakPrim qualified as WP
import Language.WeakTerm.Rule.WeakTerm qualified as WT

data EvalBase
  = VarLocal Ident
  | VarGlobal DD.DefiniteDescription
  | Hole HID.HoleID [WT.WeakTerm]
  | Prim (WP.WeakPrim WT.WeakTerm)

type EvalCtx = Cofree EvalCtxF Hint

data EvalCtxF a
  = Base
  | PiElim a [WT.WeakTerm]

type Stuck = (EvalBase, EvalCtx)

asStuckedTerm :: WT.WeakTerm -> Maybe Stuck
asStuckedTerm term =
  case term of
    m :< WT.Var x ->
      Just (VarLocal x, m :< Base)
    m :< WT.VarGlobal _ g ->
      Just (VarGlobal g, m :< Base)
    m :< WT.Hole h es ->
      Just (Hole h es, m :< Base)
    m :< WT.Prim prim ->
      Just (Prim prim, m :< Base)
    m :< WT.PiElim e es -> do
      (base, ctx) <- asStuckedTerm e
      return (base, m :< PiElim ctx es)
    _ ->
      Nothing

resume :: WT.WeakTerm -> EvalCtx -> WT.WeakTerm
resume e ctx =
  case ctx of
    _ :< Base ->
      e
    m :< PiElim ctx' args ->
      m :< WT.PiElim (resume e ctx') args -- inferred pi-elims are explicit

asPairList :: EvalCtx -> EvalCtx -> Maybe [C.Constraint]
asPairList ctx1 ctx2 =
  case (ctx1, ctx2) of
    (_ :< Base, _ :< Base) ->
      Just []
    (_ :< PiElim ctx1' args1, _ :< PiElim ctx2' args2)
      | length args1 /= length args2 ->
          Nothing
      | otherwise -> do
          pairList <- asPairList ctx1' ctx2'
          return $ zipWith C.Eq args1 args2 ++ pairList
    _ ->
      Nothing
