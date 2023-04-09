module Scene.Parse.Discern.Noema
  ( castToNoema,
    castFromNoema,
    castToNoemaIfNecessary,
    castFromNoemaIfNecessary,
  )
where

import Context.App
import Context.Gensym qualified as Gensym
import Control.Comonad.Cofree hiding (section)
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
      let tNoema = m :< WT.Noema t
      return $ m :< WT.Magic (M.Cast t tNoema e)

castFromNoema :: NominalEnv -> Mutability -> WT.WeakTerm -> App WT.WeakTerm
castFromNoema nenv mutability e@(m :< _) = do
  t <- Gensym.newHole m (asHoleArgs nenv)
  case mutability of
    Mutable ->
      return $ m :< WT.CellElim e
    Immutable -> do
      let tNoema = m :< WT.Noema t
      return $ m :< WT.Magic (M.Cast tNoema t e)

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
