module Entity.Stuck where

import Control.Comonad.Cofree
import Entity.Constraint qualified as C
import Entity.DecisionTree qualified as DT
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.HoleID qualified as HID
import Entity.Ident
import Entity.Noema qualified as N
import Entity.WeakPrim qualified as WP
import Entity.WeakTerm qualified as WT

data EvalBase
  = VarLocal Ident
  | VarGlobal DD.DefiniteDescription
  | Hole HID.HoleID [WT.WeakTerm]
  | Prim (WP.WeakPrim WT.WeakTerm)

type EvalCtx = Cofree EvalCtxF Hint

data EvalCtxF a
  = Base
  | PiElim a [WT.WeakTerm]
  | DataElim N.IsNoetic (Ident, a, WT.WeakTerm) (DT.DecisionTree WT.WeakTerm)

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
    m :< WT.DataElim isNoetic [(o, e, t)] decisionTree -> do
      (base, ctx) <- asStuckedTerm e
      return (base, m :< DataElim isNoetic (o, ctx, t) decisionTree)
    _ ->
      Nothing

resume :: WT.WeakTerm -> EvalCtx -> WT.WeakTerm
resume e ctx =
  case ctx of
    _ :< Base ->
      e
    m :< PiElim ctx' args ->
      m :< WT.PiElim (resume e ctx') args -- inferred pi-elims are explicit
    m :< DataElim isNoetic (o, ctx', t) decisionTree ->
      m :< WT.DataElim isNoetic [(o, resume e ctx', t)] decisionTree

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
