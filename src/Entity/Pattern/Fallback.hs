module Entity.Pattern.Fallback (getFallbackMatrix, Context) where

import qualified Context.Gensym as Gensym
import qualified Context.Throw as Throw
import Control.Comonad.Cofree
import qualified Data.Vector as V
import Entity.Ident
import Entity.Pattern
import qualified Entity.WeakTerm as WT

class (Gensym.Context m, Throw.Context m) => Context m

-- `cursor` is the variable `x` in `match x, y, z with (...) end`.
getFallbackMatrix ::
  Context m =>
  Ident ->
  PatternMatrix ([Ident], WT.WeakTerm) ->
  m (PatternMatrix ([Ident], WT.WeakTerm))
getFallbackMatrix cursor mat = do
  mapMaybeRowM (fallbackRow cursor) mat

fallbackRow ::
  Context m =>
  Ident ->
  PatternRow ([Ident], WT.WeakTerm) ->
  m (Maybe (PatternRow ([Ident], WT.WeakTerm)))
fallbackRow cursor (patternVector, (freedVars, body)) =
  case V.uncons patternVector of
    Nothing ->
      Throw.raiseCritical' "specialization against the empty pattern matrix shouldn't happen"
    Just ((m, WildcardVar), _) ->
      Throw.raiseCritical m "specialization against a wildcard shouldn't happen"
    Just ((m, Var x), rest) -> do
      h <- Gensym.newAster m []
      let body' = m :< WT.Let (m, x, h) (m :< WT.Var cursor) body
      return $ Just (rest, (freedVars, body'))
    Just ((_, Cons {}), _) ->
      return Nothing
