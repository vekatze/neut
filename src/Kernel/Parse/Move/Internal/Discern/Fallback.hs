module Kernel.Parse.Move.Internal.Discern.Fallback (getFallbackMatrix) where

import Control.Comonad.Cofree
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Vector qualified as V
import Error.Rule.EIO (EIO)
import Kernel.Parse.Move.Internal.Discern.Handle qualified as H
import Kernel.Parse.Move.Internal.Discern.Noema
import Kernel.Rule.Pattern
import Language.Common.Move.Raise (raiseCritical')
import Language.Common.Rule.Binder
import Language.Common.Rule.Ident
import Language.Common.Rule.Noema qualified as N
import Language.WeakTerm.Move.CreateHole qualified as WT
import Language.WeakTerm.Rule.WeakTerm qualified as WT

-- `cursor` is the variable `x` in `match x, y, z with (...) end`.
getFallbackMatrix ::
  H.Handle ->
  N.IsNoetic ->
  Ident ->
  PatternMatrix ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm) ->
  EIO (PatternMatrix ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm))
getFallbackMatrix h isNoetic cursor mat = do
  mapMaybeRowM (fallbackRow h isNoetic cursor) mat

fallbackRow ::
  H.Handle ->
  N.IsNoetic ->
  Ident ->
  PatternRow ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm) ->
  EIO (Maybe (PatternRow ([Ident], [(BinderF WT.WeakTerm, WT.WeakTerm)], WT.WeakTerm)))
fallbackRow h isNoetic cursor (patternVector, (freedVars, baseSeq, body@(mBody :< _))) =
  case V.uncons patternVector of
    Nothing ->
      raiseCritical' "Defaulting against the empty pattern matrix should not happen"
    Just ((_, WildcardVar), rest) ->
      return $ Just (rest, (freedVars, baseSeq, body))
    Just ((_, Var x), rest) -> do
      hole <- liftIO $ WT.createHole (H.gensymHandle h) mBody []
      adjustedCursor <- liftIO $ castToNoemaIfNecessary h isNoetic (mBody :< WT.Var cursor)
      return $ Just (rest, (freedVars, ((mBody, x, hole), adjustedCursor) : baseSeq, body))
    Just ((_, Cons {}), _) ->
      return Nothing
    Just ((_, Literal {}), _) ->
      return Nothing
