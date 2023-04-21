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
import Control.Comonad.Cofree hiding (section)
import Entity.Ident
import Entity.Magic qualified as M
import Entity.Mutability
import Entity.Noema qualified as N
import Entity.WeakTerm qualified as WT

castToNoema :: Mutability -> WT.WeakTerm -> App WT.WeakTerm
castToNoema mutability e@(m :< _) = do
  t <- Gensym.newHole m []
  case mutability of
    Mutable ->
      return $ m :< WT.CellIntro e
    Immutable -> do
      return $ m :< WT.Magic (M.Cast t (m :< WT.Noema t) e)

castFromNoema :: Mutability -> WT.WeakTerm -> App WT.WeakTerm
castFromNoema mutability e@(m :< _) = do
  t <- Gensym.newHole m []
  case mutability of
    Mutable ->
      return $ m :< WT.CellElim e
    Immutable -> do
      return $ m :< WT.Magic (M.Cast (m :< WT.Noema t) t e)

castToNoemaIfNecessary :: N.IsNoetic -> WT.WeakTerm -> App WT.WeakTerm
castToNoemaIfNecessary isNoetic e =
  if isNoetic
    then castToNoema Immutable e
    else return e

castFromNoemaIfNecessary :: N.IsNoetic -> WT.WeakTerm -> App WT.WeakTerm
castFromNoemaIfNecessary isNoetic e =
  if isNoetic
    then castFromNoema Immutable e
    else return e

attachPrefix :: [(Mutability, Ident, WT.WeakTerm)] -> WT.WeakTerm -> App WT.WeakTerm
attachPrefix binder cont@(m :< _) =
  case binder of
    [] ->
      return cont
    (mutability, y, e) : rest -> do
      e' <- castToNoema mutability e
      cont' <- attachPrefix rest cont
      h <- Gensym.newHole m []
      return $ m :< WT.Let WT.Opaque (m, y, h) e' cont'

attachSuffix :: [(Mutability, Ident, Ident)] -> WT.WeakTerm -> App WT.WeakTerm
attachSuffix binder cont@(m :< _) =
  case binder of
    [] ->
      return cont
    (mutability, yCont, yLocal) : rest -> do
      yLocal' <- castFromNoema mutability (m :< WT.Var yLocal)
      cont' <- attachSuffix rest cont
      h <- Gensym.newHole m []
      return $ m :< WT.Let WT.Opaque (m, yCont, h) yLocal' cont'
