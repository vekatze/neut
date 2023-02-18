module Scene.Parse.Discern.Specialize (specialize) where

import Context.App
import Context.Enum qualified as Enum
import Context.Gensym qualified as Gensym
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Data.Vector qualified as V
import Entity.Arity qualified as A
import Entity.DefiniteDescription qualified as DD
import Entity.Ident
import Entity.NominalEnv
import Entity.Pattern
import Entity.WeakTerm qualified as WT

-- `cursor` is the variable `x` in `match x, y, z with (...) end`.
specialize ::
  NominalEnv ->
  Ident ->
  (DD.DefiniteDescription, A.Arity) ->
  PatternMatrix ([Ident], WT.WeakTerm) ->
  App (PatternMatrix ([Ident], WT.WeakTerm))
specialize nenv cursor cons mat = do
  mapMaybeRowM (specializeRow nenv cursor cons) mat

specializeRow ::
  NominalEnv ->
  Ident ->
  (DD.DefiniteDescription, A.Arity) ->
  PatternRow ([Ident], WT.WeakTerm) ->
  App (Maybe (PatternRow ([Ident], WT.WeakTerm)))
specializeRow nenv cursor (dd, arity) (patternVector, (freedVars, body)) =
  case V.uncons patternVector of
    Nothing ->
      Throw.raiseCritical' "specialization against the empty pattern matrix shouldn't happen"
    Just ((m, WildcardVar), rest) -> do
      let wildcards = V.fromList $ replicate (fromInteger $ A.reify arity) (m, WildcardVar)
      return $ Just (V.concat [wildcards, rest], (freedVars, body))
    Just ((m, Var x), rest) -> do
      let wildcards = V.fromList $ replicate (fromInteger $ A.reify arity) (m, WildcardVar)
      h <- Gensym.newHole m (asHoleArgs nenv)
      let body' = m :< WT.Let WT.Transparent (m, x, h) (m :< WT.Var cursor) body
      -- let body' = m :< WT.Let (m, cursor, h) (m :< WT.Var x) body
      return $ Just (V.concat [wildcards, rest], (freedVars, body'))
    Just ((_, Cons dd' _ _ _ args), rest) ->
      if dd == dd'
        then do
          b <- Enum.isMember dd'
          if b
            then return $ Just (V.concat [V.fromList args, rest], (freedVars, body))
            else return $ Just (V.concat [V.fromList args, rest], (cursor : freedVars, body))
        else return Nothing
