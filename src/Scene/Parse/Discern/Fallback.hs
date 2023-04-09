module Scene.Parse.Discern.Fallback (getFallbackMatrix) where

import Context.App
import Context.Gensym qualified as Gensym
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Data.Vector qualified as V
import Entity.Ident
import Entity.Noema qualified as N
import Entity.NominalEnv
import Entity.Pattern
import Entity.WeakTerm qualified as WT
import Scene.Parse.Discern.Noema

-- `cursor` is the variable `x` in `match x, y, z with (...) end`.
getFallbackMatrix ::
  N.IsNoetic ->
  NominalEnv ->
  Ident ->
  PatternMatrix ([Ident], WT.WeakTerm) ->
  App (PatternMatrix ([Ident], WT.WeakTerm))
getFallbackMatrix isNoetic nenv cursor mat = do
  mapMaybeRowM (fallbackRow isNoetic nenv cursor) mat

fallbackRow ::
  N.IsNoetic ->
  NominalEnv ->
  Ident ->
  PatternRow ([Ident], WT.WeakTerm) ->
  App (Maybe (PatternRow ([Ident], WT.WeakTerm)))
fallbackRow isNoetic nenv cursor (patternVector, (freedVars, body)) =
  case V.uncons patternVector of
    Nothing ->
      Throw.raiseCritical' "defaulting against the empty pattern matrix shouldn't happen"
    Just ((_, WildcardVar), rest) ->
      return $ Just (rest, (freedVars, body))
    Just ((m, Var x), rest) -> do
      h <- Gensym.newHole m (asHoleArgs nenv)
      adjustedCursor <- castToNoemaIfNecessary nenv isNoetic (m :< WT.Var cursor)
      let body' = m :< WT.Let WT.Transparent (m, x, h) adjustedCursor body
      return $ Just (rest, (freedVars, body'))
    Just ((_, Cons {}), _) ->
      return Nothing
