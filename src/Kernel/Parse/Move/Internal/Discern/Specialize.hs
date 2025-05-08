module Kernel.Parse.Move.Internal.Discern.Specialize
  ( specialize,
    Specializer (..),
  )
where

import Control.Comonad.Cofree
import Control.Monad.IO.Class
import Data.Vector qualified as V
import Kernel.Common.Move.Handle.Global.OptimizableData qualified as OptimizableData
import Kernel.Common.Rule.OptimizableData qualified as OD
import Kernel.Parse.Move.Internal.Discern.Handle qualified as H
import Kernel.Parse.Move.Internal.Discern.Noema
import Kernel.Parse.Rule.Pattern
import Language.Common.Move.Raise (raiseCritical')
import Language.Common.Rule.ArgNum qualified as AN
import Language.Common.Rule.Binder
import Language.Common.Rule.Ident
import Language.Common.Rule.Noema qualified as N
import Language.WeakTerm.Move.CreateHole qualified as WT
import Language.WeakTerm.Rule.WeakTerm qualified as WT
import Library.Error.Rule.EIO (EIO)

-- `cursor` is the variable `x` in `match x, y, z with (...) end`.
specialize ::
  H.Handle ->
  N.IsNoetic ->
  Ident ->
  Specializer ->
  PatternMatrix ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm) ->
  EIO (PatternMatrix ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm))
specialize h isNoetic cursor cons mat = do
  mapMaybeRowM (specializeRow h isNoetic cursor cons) mat

specializeRow ::
  H.Handle ->
  N.IsNoetic ->
  Ident ->
  Specializer ->
  PatternRow ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm) ->
  EIO (Maybe (PatternRow ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm)))
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
