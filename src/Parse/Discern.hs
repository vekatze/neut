module Parse.Discern
  ( discern,
    discernIdentPlus,
    discernDef,
  )
where

import Control.Monad.State.Lazy
import Data.Basic
import Data.Env
import qualified Data.HashMap.Lazy as Map
import Data.Log
import Data.Namespace
import qualified Data.Text as T
import Data.WeakTerm
import Preprocess.Discern

type NameEnv = Map.HashMap T.Text Ident

discern :: WeakTermPlus -> WithEnv WeakTermPlus
discern e = do
  nenv <- gets topNameEnv
  discern' nenv e

discernIdentPlus :: WeakIdentPlus -> WithEnv WeakIdentPlus
discernIdentPlus (m, x, t) = do
  nenv <- gets topNameEnv
  when (Map.member (asText x) nenv) $
    raiseError m $ "the variable `" <> asText x <> "` is already defined at the top level"
  t' <- discern' nenv t
  x' <- newNameWith x
  modify (\env -> env {topNameEnv = Map.insert (asText x) x' (topNameEnv env)})
  return (m, x', t')

discernDef :: Def -> WithEnv Def
discernDef (m, xt, xts, e) = do
  nenv <- gets topNameEnv
  (xt', xts', e') <- discernFix nenv xt xts e
  return (m, xt', xts', e')

-- Alpha-convert all the variables so that different variables have different names.
discern' :: NameEnv -> WeakTermPlus -> WithEnv WeakTermPlus
discern' nenv term =
  case term of
    (m, WeakTermTau) ->
      return (m, WeakTermTau)
    (m, WeakTermUpsilon (I (s, _))) ->
      tryCand (resolveSymbol (asWeakVar m nenv) s) $
        tryCand (resolveSymbol (asWeakEnumValue m) s) $
          tryCand (resolveSymbol (asWeakEnumType m) s) $
            tryCand (resolveSymbol (asWeakConstant m) s) $
              raiseError m $ "undefined variable: " <> s
    (m, WeakTermPi xts t) -> do
      (xts', t') <- discernBinder nenv xts t
      return (m, WeakTermPi xts' t')
    (m, WeakTermPiIntro xts e) -> do
      (xts', e') <- discernBinder nenv xts e
      return (m, WeakTermPiIntro xts' e')
    (m, WeakTermPiElim e es) -> do
      es' <- mapM (discern' nenv) es
      e' <- discern' nenv e
      return (m, WeakTermPiElim e' es')
    (m, WeakTermFix (mx, x, t) xts e) -> do
      (xt', xts', e') <- discernFix nenv (mx, x, t) xts e
      return (m, WeakTermFix xt' xts' e')
    (m, WeakTermConst x) ->
      return (m, WeakTermConst x)
    (m, WeakTermAster h) ->
      return (m, WeakTermAster h)
    (m, WeakTermInt t x) -> do
      t' <- discern' nenv t
      return (m, WeakTermInt t' x)
    (m, WeakTermFloat t x) -> do
      t' <- discern' nenv t
      return (m, WeakTermFloat t' x)
    (m, WeakTermEnum s) ->
      return (m, WeakTermEnum s)
    (m, WeakTermEnumIntro x) ->
      return (m, WeakTermEnumIntro x)
    (m, WeakTermEnumElim (e, t) caseList) -> do
      e' <- discern' nenv e
      t' <- discern' nenv t
      caseList' <-
        forM caseList $ \((mCase, l), body) -> do
          l' <- discernEnumCase mCase l
          body' <- discern' nenv body
          return ((mCase, l'), body')
      return (m, WeakTermEnumElim (e', t') caseList')
    (m, WeakTermQuestion e t) -> do
      e' <- discern' nenv e
      t' <- discern' nenv t
      return (m, WeakTermQuestion e' t')
    (m, WeakTermDerangement i resultType ekts) -> do
      resultType' <- discern' nenv resultType
      let (es, ks, ts) = unzip3 ekts
      es' <- mapM (discern' nenv) es
      ts' <- mapM (discern' nenv) ts
      return (m, WeakTermDerangement i resultType' (zip3 es' ks ts'))

discernBinder ::
  NameEnv ->
  [WeakIdentPlus] ->
  WeakTermPlus ->
  WithEnv ([WeakIdentPlus], WeakTermPlus)
discernBinder nenv binder e =
  case binder of
    [] -> do
      e' <- discern' nenv e
      return ([], e')
    (mx, x, t) : xts -> do
      t' <- discern' nenv t
      x' <- newNameWith x
      (xts', e') <- discernBinder (Map.insert (asText x) x' nenv) xts e
      return ((mx, x', t') : xts', e')

discernFix ::
  NameEnv ->
  WeakIdentPlus ->
  [WeakIdentPlus] ->
  WeakTermPlus ->
  WithEnv (WeakIdentPlus, [WeakIdentPlus], WeakTermPlus)
discernFix nenv self binder e = do
  (binder', e') <- discernBinder nenv (self : binder) e
  return (head binder', tail binder', e')
