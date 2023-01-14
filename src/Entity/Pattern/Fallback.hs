module Entity.Pattern.Fallback (getFallbackMatrix, Context) where

import qualified Context.Gensym as Gensym
import qualified Context.Throw as Throw
import Control.Comonad.Cofree
import qualified Data.Vector as V
import Entity.Ident
import Entity.NominalEnv
import Entity.Pattern
import qualified Entity.WeakTerm as WT

class (Gensym.Context m, Throw.Context m) => Context m

-- `cursor` is the variable `x` in `match x, y, z with (...) end`.
getFallbackMatrix ::
  Context m =>
  NominalEnv ->
  Ident ->
  PatternMatrix ([Ident], WT.WeakTerm) ->
  m (PatternMatrix ([Ident], WT.WeakTerm))
getFallbackMatrix nenv cursor mat = do
  mapMaybeRowM (fallbackRow nenv cursor) mat

fallbackRow ::
  Context m =>
  NominalEnv ->
  Ident ->
  PatternRow ([Ident], WT.WeakTerm) ->
  m (Maybe (PatternRow ([Ident], WT.WeakTerm)))
fallbackRow nenv cursor (patternVector, (freedVars, body)) =
  case V.uncons patternVector of
    Nothing ->
      Throw.raiseCritical' "defaulting against the empty pattern matrix shouldn't happen"
    Just ((_, WildcardVar), rest) ->
      return $ Just (rest, (freedVars, body))
    Just ((m, Var x), rest) -> do
      h <- Gensym.newHole m (asHoleArgs nenv)
      let body' = m :< WT.Let WT.Transparent (m, x, h) (m :< WT.Var cursor) body
      return $ Just (rest, (freedVars, body'))
    Just ((_, Cons {}), _) ->
      return Nothing
