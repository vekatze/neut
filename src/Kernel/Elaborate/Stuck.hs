module Kernel.Elaborate.Stuck
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
import Kernel.Elaborate.Constraint qualified as C
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.DefaultArgs qualified as DefaultArgs
import Language.Common.Ident
import Language.Common.ImpArgs qualified as ImpArgs
import Language.WeakTerm.WeakPrimValue qualified as WPV
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Hint

data EvalBase
  = VarLocal Ident
  | VarGlobal DD.DefiniteDescription
  | Prim (WPV.WeakPrimValue WT.WeakType)

type EvalCtx = Cofree EvalCtxF Hint

data EvalCtxF a
  = Base
  | PiElim a (ImpArgs.ImpArgs WT.WeakType) (DefaultArgs.DefaultArgs WT.WeakTerm) [WT.WeakTerm]

type Stuck = (EvalBase, EvalCtx)

asStuckedTerm :: WT.WeakTerm -> Maybe Stuck
asStuckedTerm term =
  case term of
    m :< WT.Var x ->
      Just (VarLocal x, m :< Base)
    m :< WT.VarGlobal _ g ->
      Just (VarGlobal g, m :< Base)
    m :< WT.Prim prim ->
      Just (Prim prim, m :< Base)
    m :< WT.PiElim False e impArgs defaultArgs expArgs -> do
      (base, ctx) <- asStuckedTerm e
      return (base, m :< PiElim ctx impArgs defaultArgs expArgs)
    _ ->
      Nothing

resume :: WT.WeakTerm -> EvalCtx -> WT.WeakTerm
resume e ctx =
  case ctx of
    _ :< Base ->
      e
    m :< PiElim ctx' impArgs defaultArgs expArgs ->
      m :< WT.PiElim False (resume e ctx') impArgs defaultArgs expArgs -- inferred pi-elims are explicit

asPairList :: EvalCtx -> EvalCtx -> Maybe [C.Constraint]
asPairList ctx1 ctx2 =
  case (ctx1, ctx2) of
    (_ :< Base, _ :< Base) ->
      Just []
    (_ :< PiElim ctx1' impArgs1 _ expArgs1, _ :< PiElim ctx2' impArgs2 _ expArgs2)
      | ImpArgs.Unspecified <- impArgs1,
        ImpArgs.FullySpecified _ <- impArgs2 ->
          Nothing
      | ImpArgs.FullySpecified _ <- impArgs1,
        ImpArgs.Unspecified <- impArgs2 ->
          Nothing
      | length expArgs1 /= length expArgs2 ->
          Nothing
      | ImpArgs.FullySpecified impArgs1' <- impArgs1,
        ImpArgs.FullySpecified impArgs2' <- impArgs2 -> do
          pairList <- asPairList ctx1' ctx2'
          return $ zipWith C.Eq impArgs1' impArgs2' ++ pairList
      | ImpArgs.Unspecified <- impArgs1,
        ImpArgs.Unspecified <- impArgs2 -> do
          pairList <- asPairList ctx1' ctx2'
          return pairList
    _ ->
      Nothing
