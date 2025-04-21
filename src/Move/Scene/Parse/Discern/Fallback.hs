module Move.Scene.Parse.Discern.Fallback (getFallbackMatrix) where

import Move.Context.App
import Move.Context.Gensym qualified as Gensym
import Move.Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Data.Vector qualified as V
import Rule.Binder
import Rule.Ident
import Rule.Noema qualified as N
import Rule.Pattern
import Rule.WeakTerm qualified as WT
import Move.Scene.Parse.Discern.Noema

-- `cursor` is the variable `x` in `match x, y, z with (...) end`.
getFallbackMatrix ::
  N.IsNoetic ->
  Ident ->
  PatternMatrix ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm) ->
  App (PatternMatrix ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm))
getFallbackMatrix isNoetic cursor mat = do
  mapMaybeRowM (fallbackRow isNoetic cursor) mat

fallbackRow ::
  N.IsNoetic ->
  Ident ->
  PatternRow ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm) ->
  App (Maybe (PatternRow ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm)))
fallbackRow isNoetic cursor (patternVector, (freedVars, baseSeq, body@(mBody :< _))) =
  case V.uncons patternVector of
    Nothing ->
      Throw.raiseCritical' "Defaulting against the empty pattern matrix should not happen"
    Just ((_, WildcardVar), rest) ->
      return $ Just (rest, (freedVars, baseSeq, body))
    Just ((_, Var x), rest) -> do
      h <- Gensym.newHole mBody []
      adjustedCursor <- castToNoemaIfNecessary isNoetic (mBody :< WT.Var cursor)
      return $ Just (rest, (freedVars, ((mBody, x, h), adjustedCursor) : baseSeq, body))
    Just ((_, Cons {}), _) ->
      return Nothing
    Just ((_, Literal {}), _) ->
      return Nothing
