module Scene.Parse.Discern.Specialize (specialize) where

import Context.App
import Context.Gensym qualified as Gensym
import Context.OptimizableData qualified as OptimizableData
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Data.Vector qualified as V
import Entity.ArgNum qualified as AN
import Entity.Binder
import Entity.DefiniteDescription qualified as DD
import Entity.Ident
import Entity.Noema qualified as N
import Entity.OptimizableData qualified as OD
import Entity.Pattern
import Entity.WeakTerm qualified as WT
import Scene.Parse.Discern.Noema

-- `cursor` is the variable `x` in `match x, y, z with (...) end`.
specialize ::
  N.IsNoetic ->
  Ident ->
  (DD.DefiniteDescription, AN.ArgNum) ->
  PatternMatrix ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm) ->
  App (PatternMatrix ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm))
specialize isNoetic cursor cons mat = do
  mapMaybeRowM (specializeRow isNoetic cursor cons) mat

specializeRow ::
  N.IsNoetic ->
  Ident ->
  (DD.DefiniteDescription, AN.ArgNum) ->
  PatternRow ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm) ->
  App (Maybe (PatternRow ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm)))
specializeRow isNoetic cursor (dd, argNum) (patternVector, (freedVars, baseSeq, body@(mBody :< _))) =
  case V.uncons patternVector of
    Nothing ->
      Throw.raiseCritical' "specialization against the empty pattern matrix shouldn't happen"
    Just ((m, WildcardVar), rest) -> do
      let wildcards = V.fromList $ replicate (AN.reify argNum) (m, WildcardVar)
      return $ Just (V.concat [wildcards, rest], (freedVars, baseSeq, body))
    Just ((_, Var x), rest) -> do
      let wildcards = V.fromList $ replicate (AN.reify argNum) (mBody, WildcardVar)
      h <- Gensym.newHole mBody []
      adjustedCursor <- castToNoemaIfNecessary isNoetic (mBody :< WT.Var cursor)
      return $ Just (V.concat [wildcards, rest], (freedVars, ((mBody, x, h), adjustedCursor) : baseSeq, body))
    Just ((_, Cons (ConsInfo {..})), rest) ->
      if dd == consDD
        then do
          od <- OptimizableData.lookup consDD
          case od of
            Just OD.Enum ->
              return $ Just (V.concat [V.fromList args, rest], (freedVars, baseSeq, body))
            Just OD.Unary ->
              return $ Just (V.concat [V.fromList args, rest], (freedVars, baseSeq, body))
            _ ->
              return $ Just (V.concat [V.fromList args, rest], (cursor : freedVars, baseSeq, body))
        else return Nothing
