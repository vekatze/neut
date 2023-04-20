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
import Entity.NominalEnv
import Entity.WeakTerm qualified as WT

castToNoema :: NominalEnv -> Mutability -> WT.WeakTerm -> App WT.WeakTerm
castToNoema nenv mutability e@(m :< _) = do
  t <- Gensym.newHole m (asHoleArgs nenv)
  case mutability of
    Mutable ->
      return $ m :< WT.CellIntro e
    Immutable -> do
      return $ m :< WT.Magic (M.Cast t (m :< WT.Noema t) e)

castFromNoema :: NominalEnv -> Mutability -> WT.WeakTerm -> App WT.WeakTerm
castFromNoema nenv mutability e@(m :< _) = do
  t <- Gensym.newHole m (asHoleArgs nenv)
  case mutability of
    Mutable ->
      return $ m :< WT.CellElim e
    Immutable -> do
      return $ m :< WT.Magic (M.Cast (m :< WT.Noema t) t e)

castToNoemaIfNecessary :: NominalEnv -> N.IsNoetic -> WT.WeakTerm -> App WT.WeakTerm
castToNoemaIfNecessary nenv isNoetic e =
  if isNoetic
    then castToNoema nenv Immutable e
    else return e

castFromNoemaIfNecessary :: NominalEnv -> N.IsNoetic -> WT.WeakTerm -> App WT.WeakTerm
castFromNoemaIfNecessary nenv isNoetic e =
  if isNoetic
    then castFromNoema nenv Immutable e
    else return e

attachPrefix :: NominalEnv -> [(Mutability, Ident, WT.WeakTerm)] -> WT.WeakTerm -> App WT.WeakTerm
attachPrefix nenv binder cont@(m :< _) =
  case binder of
    [] ->
      return cont
    (mutability, y, e) : rest -> do
      e' <- castToNoema nenv mutability e
      cont' <- attachPrefix nenv rest cont
      h <- Gensym.newHole m (asHoleArgs nenv)
      return $ m :< WT.Let WT.Opaque (m, y, h) e' cont'

attachSuffix :: NominalEnv -> [(Mutability, Ident, Ident)] -> WT.WeakTerm -> App WT.WeakTerm
attachSuffix nenv binder cont@(m :< _) =
  case binder of
    [] ->
      return cont
    (mutability, yCont, yLocal) : rest -> do
      yLocal' <- castFromNoema nenv mutability (m :< WT.Var yLocal)
      cont' <- attachSuffix nenv rest cont
      h <- Gensym.newHole m (asHoleArgs nenv)
      return $ m :< WT.Let WT.Opaque (m, yCont, h) yLocal' cont'
