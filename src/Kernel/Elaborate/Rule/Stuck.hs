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

import Control.Comonad.Cofree
import Kernel.Elaborate.Rule.Constraint qualified as C
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.HoleID qualified as HID
import Language.Common.Rule.Ident
import Language.WeakTerm.Rule.WeakPrim qualified as WP
import Language.WeakTerm.Rule.WeakTerm qualified as WT
import Logger.Rule.Hint

data EvalBase
  = VarLocal Ident
  | VarGlobal DD.DefiniteDescription
  | Hole HID.HoleID [WT.WeakTerm]
  | Prim (WP.WeakPrim WT.WeakTerm)

type EvalCtx = Cofree EvalCtxF Hint

data EvalCtxF a
  = Base
  | PiElim a (Maybe [WT.WeakTerm]) [WT.WeakTerm]

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
    m :< WT.PiElim False e impArgs expArgs -> do
      (base, ctx) <- asStuckedTerm e
      return (base, m :< PiElim ctx impArgs expArgs)
    _ ->
      Nothing

resume :: WT.WeakTerm -> EvalCtx -> WT.WeakTerm
resume e ctx =
  case ctx of
    _ :< Base ->
      e
    m :< PiElim ctx' impArgs expArgs ->
      m :< WT.PiElim False (resume e ctx') impArgs expArgs -- inferred pi-elims are explicit

asPairList :: EvalCtx -> EvalCtx -> Maybe [C.Constraint]
asPairList ctx1 ctx2 =
  case (ctx1, ctx2) of
    (_ :< Base, _ :< Base) ->
      Just []
    (_ :< PiElim ctx1' impArgs1 expArgs1, _ :< PiElim ctx2' impArgs2 expArgs2)
      | Nothing <- impArgs1,
        Just _ <- impArgs2 ->
          Nothing
      | Just _ <- impArgs1,
        Nothing <- impArgs2 ->
          Nothing
      | length expArgs1 /= length expArgs2 ->
          Nothing
      | Just impArgs1' <- impArgs1,
        Just impArgs2' <- impArgs2 -> do
          pairList <- asPairList ctx1' ctx2'
          return $ zipWith C.Eq impArgs1' impArgs2' ++ zipWith C.Eq expArgs1 expArgs2 ++ pairList
      | Nothing <- impArgs1,
        Nothing <- impArgs2 -> do
          pairList <- asPairList ctx1' ctx2'
          return $ zipWith C.Eq expArgs1 expArgs2 ++ pairList
    _ ->
      Nothing
