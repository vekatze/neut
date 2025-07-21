module Kernel.Parse.Internal.Discern.Specialize
  ( specialize,
    Specializer (..),
  )
where

import App.App (App)
import App.Run (raiseCritical')
import Control.Comonad.Cofree
import Control.Monad.IO.Class
import Data.Vector qualified as V
import Kernel.Common.Handle.Global.OptimizableData qualified as OptimizableData
import Kernel.Common.OptimizableData qualified as OD
import Kernel.Parse.Internal.Discern.Handle qualified as H
import Kernel.Parse.Internal.Discern.Noema
import Kernel.Parse.Pattern
import Language.Common.ArgNum qualified as AN
import Language.Common.Binder
import Language.Common.Ident
import Language.Common.Noema qualified as N
import Language.WeakTerm.CreateHole qualified as WT
import Language.WeakTerm.WeakTerm qualified as WT

-- `cursor` is the variable `x` in `match x, y, z with (...) end`.
specialize ::
  H.Handle ->
  N.IsNoetic ->
  Ident ->
  Specializer ->
  PatternMatrix ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm) ->
  App (PatternMatrix ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm))
specialize h isNoetic cursor cons mat = do
  mapMaybeRowM (specializeRow h isNoetic cursor cons) mat

specializeRow ::
  H.Handle ->
  N.IsNoetic ->
  Ident ->
  Specializer ->
  PatternRow ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm) ->
  App (Maybe (PatternRow ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm)))
specializeRow h isNoetic cursor specializer (patternVector, (freedVars, baseSeq, body@(mBody :< _))) =
  case V.uncons patternVector of
    Nothing ->
      raiseCritical' "Specialization against the empty pattern matrix should not happen"
    Just ((m, WildcardVar), rest) -> do
      case specializer of
        LiteralSpecializer _ -> do
          return $ Just (rest, (freedVars, baseSeq, body))
        ConsSpecializer (ConsInfo {consArgNum}) -> do
          let wildcards = V.fromList $ replicate (AN.reify consArgNum) (m, WildcardVar)
          return $ Just (V.concat [wildcards, rest], (freedVars, baseSeq, body))
    Just ((_, Var x), rest) -> do
      case specializer of
        LiteralSpecializer _ -> do
          hole <- liftIO $ WT.createHole (H.gensymHandle h) mBody []
          adjustedCursor <- liftIO $ castToNoemaIfNecessary h isNoetic (mBody :< WT.Var cursor)
          return $ Just (rest, (freedVars, ((mBody, x, hole), adjustedCursor) : baseSeq, body))
        ConsSpecializer (ConsInfo {consArgNum}) -> do
          let wildcards = V.fromList $ replicate (AN.reify consArgNum) (mBody, WildcardVar)
          hole <- liftIO $ WT.createHole (H.gensymHandle h) mBody []
          adjustedCursor <- liftIO $ castToNoemaIfNecessary h isNoetic (mBody :< WT.Var cursor)
          return $ Just (V.concat [wildcards, rest], (freedVars, ((mBody, x, hole), adjustedCursor) : baseSeq, body))
    Just ((_, Cons (ConsInfo {..})), rest) -> do
      case specializer of
        LiteralSpecializer {} ->
          return Nothing
        ConsSpecializer (ConsInfo {consDD = dd}) -> do
          if dd == consDD
            then do
              od <- liftIO $ OptimizableData.lookup (H.optDataHandle h) consDD
              case od of
                Just OD.Enum ->
                  return $ Just (V.concat [V.fromList args, rest], (freedVars, baseSeq, body))
                Just OD.Unary ->
                  return $ Just (V.concat [V.fromList args, rest], (freedVars, baseSeq, body))
                _ ->
                  return $ Just (V.concat [V.fromList args, rest], (cursor : freedVars, baseSeq, body))
            else return Nothing
    Just ((_, Literal l), rest) -> do
      case specializer of
        LiteralSpecializer l' ->
          if l == l'
            then return $ Just (rest, (freedVars, baseSeq, body))
            else return Nothing
        _ ->
          return Nothing
