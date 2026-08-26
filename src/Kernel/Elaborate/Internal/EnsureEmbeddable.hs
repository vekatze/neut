module Kernel.Elaborate.Internal.EnsureEmbeddable (ensureEmbeddable) where

import App.App (App)
import App.Run (raiseError)
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Set qualified as S
import Data.Text qualified as T
import Kernel.Elaborate.Internal.Handle.Elaborate qualified as ElaborateHandle
import Kernel.Elaborate.Internal.TypeUtil qualified as TypeUtil
import Kernel.Elaborate.Trace qualified as ElaborateTrace
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Ident.Reify qualified as Ident
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.Term.Children (termChildren)
import Language.Term.FreeVars qualified as TM
import Language.Term.PrimValue qualified as PV
import Language.Term.Term qualified as TM
import Language.Term.TraceSites qualified as TraceSites
import Language.Term.Weaken (weakenType)
import Language.WeakTerm.ToText (toTextType)
import Logger.Hint

ensureEmbeddable :: ElaborateHandle.Handle -> T.Text -> TM.Term -> App ()
ensureEmbeddable h headLabel term@(m :< node) =
  case node of
    TM.EmbedIntro body -> do
      forM_ (S.toList $ TM.freeVars term) $ \x ->
        raiseError m $ "`embed` cannot capture the variable `" <> Ident.toText x <> "`"
      ensureStaticTerm h headLabel m body
    _ ->
      mapM_ (ensureEmbeddable h headLabel) $ termChildren node

ensureStaticTerm :: ElaborateHandle.Handle -> T.Text -> Hint -> TM.Term -> App ()
ensureStaticTerm h headLabel m term@(_ :< node) =
  case node of
    TM.DataIntro _ _ dataArgs _ -> do
      mapM_ (ensureEvaluatedType h m) dataArgs
      mapM_ (ensureStaticTerm h headLabel m) $ termChildren node
    TM.BoxIntroLift _ body ->
      ensureStaticTerm h headLabel m body
    TM.EmbedIntro body ->
      ensureStaticTerm h headLabel m body
    TM.Prim primValue ->
      case primValue of
        PV.Op {} ->
          raiseNonStatic m "a primitive operation"
        _ ->
          return ()
    TM.PiIntro {} ->
      mapM_ (ensureEmbeddable h headLabel) $ termChildren node
    TM.VarGlobal {} ->
      return ()
    TM.TauIntro ty ->
      ensureEvaluatedType h m ty
    TM.CodeIntro body ->
      ensureStaticTerm h headLabel m body
    TM.CodeElim _ body ->
      ensureStaticTerm h headLabel m body
    TM.Invoke _ body ->
      ensureStaticTerm h headLabel m body
    TM.Magic _ (M.LowMagic (LM.OpaqueValue body)) ->
      ensureStaticTerm h headLabel m body
    TM.Var {} ->
      raiseNonStatic m "a variable"
    _ ->
      case TraceSites.findBlocker term of
        Just blocker -> do
          traceBlock <- liftIO $ ElaborateTrace.renderFailureTrace h headLabel m blocker
          raiseError m $
            "Expected a compile-time value, but got "
              <> TraceSites.describeBlocker blocker
              <> ":\n\n"
              <> traceBlock
        Nothing ->
          raiseNonStatic m "a term that is not a value"

raiseNonStatic :: Hint -> T.Text -> App a
raiseNonStatic m description =
  raiseError m $ "Expected a compile-time value, but got " <> description

ensureEvaluatedType :: ElaborateHandle.Handle -> Hint -> TM.Type -> App ()
ensureEvaluatedType h m ty = do
  stuckOrNone <- stuckTypeOf h m S.empty ty
  case stuckOrNone of
    Nothing ->
      return ()
    Just stuck ->
      raiseError m $
        "Expected a compile-time value, but the type `"
          <> toTextType (weakenType stuck)
          <> "` in it isn't evaluated"

stuckTypeOf ::
  ElaborateHandle.Handle ->
  Hint ->
  S.Set DD.DefiniteDescription ->
  TM.Type ->
  App (Maybe TM.Type)
stuckTypeOf h m visited ty0 = do
  ty <- TypeUtil.inlineType h m ty0
  case ty of
    _ :< TM.Tau ->
      return Nothing
    _ :< TM.PrimType {} ->
      return Nothing
    _ :< TM.Void ->
      return Nothing
    _ :< TM.Resource {} ->
      return Nothing
    _ :< TM.Pi _ impArgs expArgs defaultArgs cod -> do
      let binderTypes = map (\(_, _, _, t) -> t) $ impArgs ++ expArgs ++ defaultArgs
      stuckTypeOfList h m visited $ binderTypes ++ [cod]
    _ :< TM.Box t ->
      stuckTypeOf h m visited t
    _ :< TM.BoxNoema t ->
      stuckTypeOf h m visited t
    _ :< TM.Embed t ->
      stuckTypeOf h m visited t
    _ :< TM.Code t ->
      stuckTypeOf h m visited t
    _ :< TM.Data _ dataName dataArgs -> do
      stuckOrNone <- stuckTypeOfList h m visited dataArgs
      case stuckOrNone of
        Just stuck ->
          return $ Just stuck
        Nothing
          | S.member dataName visited ->
              return Nothing
          | otherwise -> do
              consArgsList <- TypeUtil.getConsArgTypes h m dataName dataArgs
              let consArgTypes = map (\(_, _, _, t) -> t) $ concat consArgsList
              stuckTypeOfList h m (S.insert dataName visited) consArgTypes
    _ ->
      return $ Just ty

stuckTypeOfList ::
  ElaborateHandle.Handle ->
  Hint ->
  S.Set DD.DefiniteDescription ->
  [TM.Type] ->
  App (Maybe TM.Type)
stuckTypeOfList h m visited ts =
  case ts of
    [] ->
      return Nothing
    t : rest -> do
      stuckOrNone <- stuckTypeOf h m visited t
      case stuckOrNone of
        Just stuck ->
          return $ Just stuck
        Nothing ->
          stuckTypeOfList h m visited rest
