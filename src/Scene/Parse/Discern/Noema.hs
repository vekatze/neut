module Scene.Parse.Discern.Noema
  ( castToNoemaIfNecessary,
    castFromNoemaIfNecessary,
  )
where

import Context.App
import Context.Gensym qualified as Gensym
import Control.Comonad.Cofree hiding (section)
import Rule.Magic qualified as M
import Rule.Noema qualified as N
import Rule.WeakTerm qualified as WT

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

castToNoema :: WT.WeakTerm -> App WT.WeakTerm
castToNoema e@(m :< _) = do
  t <- Gensym.newHole m []
  return $ m :< WT.Magic (M.WeakMagic $ M.Cast t (m :< WT.BoxNoema t) e)

castFromNoema :: WT.WeakTerm -> App WT.WeakTerm
castFromNoema e@(m :< _) = do
  t <- Gensym.newHole m []
  return $ m :< WT.Magic (M.WeakMagic $ M.Cast (m :< WT.BoxNoema t) t e)
