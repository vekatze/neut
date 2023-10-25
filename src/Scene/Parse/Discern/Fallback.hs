module Scene.Parse.Discern.Fallback (getFallbackMatrix) where

import Context.App
import Context.Gensym qualified as Gensym
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Data.Vector qualified as V
import Entity.Ident
import Entity.Noema qualified as N
import Entity.Pattern
import Entity.WeakTerm qualified as WT
import Scene.Parse.Discern.Noema

-- `cursor` is the variable `x` in `match x, y, z with (...) end`.
getFallbackMatrix ::
  N.IsNoetic ->
  Ident ->
  PatternMatrix ([Ident], WT.WeakTerm) ->
  App (PatternMatrix ([Ident], WT.WeakTerm))
getFallbackMatrix isNoetic cursor mat = do
  mapMaybeRowM (fallbackRow isNoetic cursor) mat

fallbackRow ::
  N.IsNoetic ->
  Ident ->
  PatternRow ([Ident], WT.WeakTerm) ->
  App (Maybe (PatternRow ([Ident], WT.WeakTerm)))
fallbackRow isNoetic cursor (patternVector, (freedVars, body@(mBody :< _))) =
  case V.uncons patternVector of
    Nothing ->
      Throw.raiseCritical' "defaulting against the empty pattern matrix shouldn't happen"
    Just ((_, WildcardVar), rest) ->
      return $ Just (rest, (freedVars, body))
    Just ((_, Var x), rest) -> do
      h <- Gensym.newHole mBody []
      adjustedCursor <- castToNoemaIfNecessary isNoetic (mBody :< WT.Var cursor)
      let body' = mBody :< WT.Let WT.Clear (mBody, x, h) adjustedCursor body
      return $ Just (rest, (freedVars, body'))
    Just ((_, Cons {}), _) ->
      return Nothing
