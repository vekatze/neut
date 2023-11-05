module Scene.Parse.Discern.Noema
  ( castToNoema,
    castFromNoema,
    castToNoemaIfNecessary,
    castFromNoemaIfNecessary,
    attachPrefix,
    attachSuffix,
  )
where

import Context.App
import Context.Gensym qualified as Gensym
import Context.Tag qualified as Tag
import Control.Comonad.Cofree hiding (section)
import Entity.Hint
import Entity.Ident
import Entity.Magic qualified as M
import Entity.Noema qualified as N
import Entity.WeakTerm qualified as WT

castToNoema :: WT.WeakTerm -> App WT.WeakTerm
castToNoema e@(m :< _) = do
  t <- Gensym.newHole m []
  return $ m :< WT.Magic (M.Cast t (m :< WT.Noema t) e)

castFromNoema :: WT.WeakTerm -> App WT.WeakTerm
castFromNoema e@(m :< _) = do
  t <- Gensym.newHole m []
  return $ m :< WT.Magic (M.Cast (m :< WT.Noema t) t e)

castToNoemaIfNecessary :: N.IsNoetic -> WT.WeakTerm -> App WT.WeakTerm
castToNoemaIfNecessary isNoetic e =
  if isNoetic
    then castToNoema e
    else return e

castFromNoemaIfNecessary :: N.IsNoetic -> WT.WeakTerm -> App WT.WeakTerm
castFromNoemaIfNecessary isNoetic e =
  if isNoetic
    then castFromNoema e
    else return e

attachPrefix :: [(Ident, (Hint, WT.WeakTerm))] -> WT.WeakTerm -> App WT.WeakTerm
attachPrefix binder cont@(m :< _) =
  case binder of
    [] ->
      return cont
    (y, (mOrig, e@(mDef :< _))) : rest -> do
      e' <- castToNoema e
      cont' <- attachPrefix rest cont
      h <- Gensym.newHole m []
      Tag.insert mDef (innerLength y) mOrig
      return $ m :< WT.Let WT.Opaque (m, y, h) e' cont'

attachSuffix :: [(Ident, Ident)] -> WT.WeakTerm -> App WT.WeakTerm
attachSuffix binder cont@(m :< _) =
  case binder of
    [] ->
      return cont
    (yCont, yLocal) : rest -> do
      yLocal' <- castFromNoema (m :< WT.Var yLocal)
      cont' <- attachSuffix rest cont
      h <- Gensym.newHole m []
      return $ m :< WT.Let WT.Opaque (m, yCont, h) yLocal' cont'
