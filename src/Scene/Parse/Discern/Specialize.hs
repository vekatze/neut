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
import Entity.Noema qualified as N
import Entity.Pattern
import Entity.WeakTerm qualified as WT
import Scene.Parse.Discern.Noema

-- `cursor` is the variable `x` in `match x, y, z with (...) end`.
specialize ::
  N.IsNoetic ->
  Ident ->
  (DD.DefiniteDescription, A.Arity) ->
  PatternMatrix ([Ident], WT.WeakTerm) ->
  App (PatternMatrix ([Ident], WT.WeakTerm))
specialize isNoetic cursor cons mat = do
  mapMaybeRowM (specializeRow isNoetic cursor cons) mat

specializeRow ::
  N.IsNoetic ->
  Ident ->
  (DD.DefiniteDescription, A.Arity) ->
  PatternRow ([Ident], WT.WeakTerm) ->
  App (Maybe (PatternRow ([Ident], WT.WeakTerm)))
specializeRow isNoetic cursor (dd, arity) (patternVector, (freedVars, body@(mBody :< _))) =
  case V.uncons patternVector of
    Nothing ->
      Throw.raiseCritical' "specialization against the empty pattern matrix shouldn't happen"
    Just ((m, WildcardVar), rest) -> do
      let wildcards = V.fromList $ replicate (fromInteger $ A.reify arity) (m, WildcardVar)
      return $ Just (V.concat [wildcards, rest], (freedVars, body))
    Just ((_, Var x), rest) -> do
      let wildcards = V.fromList $ replicate (fromInteger $ A.reify arity) (mBody, WildcardVar)
      h <- Gensym.newHole mBody []
      adjustedCursor <- castToNoemaIfNecessary isNoetic (mBody :< WT.Var cursor)
      let body' = mBody :< WT.Let WT.Transparent (mBody, x, h) adjustedCursor body
      return $ Just (V.concat [wildcards, rest], (freedVars, body'))
    Just ((_, Cons dd' _ _ _ args), rest) ->
      if dd == dd'
        then do
          b <- Enum.isMember dd'
          if b
            then return $ Just (V.concat [V.fromList args, rest], (freedVars, body))
            else return $ Just (V.concat [V.fromList args, rest], (cursor : freedVars, body))
        else return Nothing
