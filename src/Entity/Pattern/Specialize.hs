module Entity.Pattern.Specialize (specialize, Context) where

import qualified Context.Gensym as Gensym
import qualified Context.Throw as Throw
import Control.Comonad.Cofree
import qualified Data.Vector as V
import qualified Entity.Arity as A
import qualified Entity.DefiniteDescription as DD
import Entity.Ident
import Entity.Pattern
import qualified Entity.WeakTerm as WT

class (Gensym.Context m, Throw.Context m) => Context m

-- `cursor` is the variable `x` in `match x, y, z with (...) end`.
specialize ::
  Context m =>
  Ident ->
  (DD.DefiniteDescription, A.Arity) ->
  PatternMatrix ([Ident], WT.WeakTerm) ->
  m (PatternMatrix ([Ident], WT.WeakTerm))
specialize cursor cons mat = do
  mapMaybeRowM (specializeRow cursor cons) mat

specializeRow ::
  Context m =>
  Ident ->
  (DD.DefiniteDescription, A.Arity) ->
  PatternRow ([Ident], WT.WeakTerm) ->
  m (Maybe (PatternRow ([Ident], WT.WeakTerm)))
specializeRow cursor (dd, arity) (patternVector, (freedVars, body)) =
  case V.uncons patternVector of
    Nothing ->
      Throw.raiseCritical' "specialization against the empty pattern matrix shouldn't happen"
    Just ((m, WildcardVar), _) ->
      Throw.raiseCritical m "specialization against a wildcard shouldn't happen"
    Just ((m, Var x), rest) -> do
      let wildcards = V.fromList $ replicate (fromInteger $ A.reify arity) (m, WildcardVar)
      h <- Gensym.newAster m []
      let body' = m :< WT.Let (m, cursor, h) (m :< WT.Var x) body
      return $ Just (V.concat [wildcards, rest], (freedVars, body'))
    Just ((_, Cons dd' _ args), rest) ->
      if dd == dd'
        then return $ Just (V.concat [V.fromList args, rest], (cursor : freedVars, body))
        else return Nothing
