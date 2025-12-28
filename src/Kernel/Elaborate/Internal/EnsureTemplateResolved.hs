module Kernel.Elaborate.Internal.EnsureTemplateResolved
  ( ensureTemplateResolved,
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Comonad.Cofree
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Kernel.Elaborate.Internal.Handle.Def qualified as Definition
import Kernel.Elaborate.Internal.Handle.Elaborate (Handle, defHandle)
import Language.Common.Binder
import Language.Common.DecisionTree qualified as DT
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Magic qualified as M
import Language.Term.Prim qualified as P
import Language.Term.PrimValue qualified as PV
import Language.Term.Term qualified as TM
import Logger.Hint

-- Ensure that all templates are fully resolved (no template functions or primitives remain).
ensureTemplateResolved :: Handle -> Hint -> TM.Term -> App ()
ensureTemplateResolved h m term =
  case term of
    _ :< TM.Tau ->
      return ()
    _ :< TM.Var {} ->
      return ()
    m' :< TM.VarGlobal _ dd -> do
      -- Check if this is a call to a template function.
      isTemplate <- liftIO $ Definition.isTemplate' (defHandle h) dd
      when isTemplate $ do
        raiseError m' $ "template function `" <> DD.reify dd <> "` cannot be called from non-template code"
    _ :< TM.Pi _ impArgs expArgs cod -> do
      forM_ impArgs $ \(binder, maybeType) -> do
        ensureTemplateResolvedInBinder h binder
        mapM_ (ensureTemplateResolved h m) maybeType
      mapM_ (ensureTemplateResolvedInBinder h) expArgs
      ensureTemplateResolved h m cod
    _ :< TM.PiIntro _ impArgs expArgs e -> do
      forM_ impArgs $ \(binder, maybeType) -> do
        ensureTemplateResolvedInBinder h binder
        mapM_ (ensureTemplateResolved h m) maybeType
      mapM_ (ensureTemplateResolvedInBinder h) expArgs
      ensureTemplateResolved h m e
    _ :< TM.PiElim _ e impArgs expArgs -> do
      ensureTemplateResolved h m e
      mapM_ (ensureTemplateResolved h m) impArgs
      mapM_ (ensureTemplateResolved h m) expArgs
    _ :< TM.Data _ _ es ->
      mapM_ (ensureTemplateResolved h m) es
    _ :< TM.DataIntro _ _ dataArgs consArgs -> do
      mapM_ (ensureTemplateResolved h m) dataArgs
      mapM_ (ensureTemplateResolved h m) consArgs
    _ :< TM.DataElim _ oets decisionTree -> do
      forM_ oets $ \(_, e, t) -> do
        ensureTemplateResolved h m e
        ensureTemplateResolved h m t
      ensureTemplateResolvedInDecisionTree h decisionTree
    _ :< TM.Box t ->
      ensureTemplateResolved h m t
    _ :< TM.BoxNoema t ->
      ensureTemplateResolved h m t
    _ :< TM.BoxIntro letSeq e -> do
      forM_ letSeq $ \(binder, term') -> do
        ensureTemplateResolvedInBinder h binder
        ensureTemplateResolved h m term'
      ensureTemplateResolved h m e
    _ :< TM.BoxElim castSeq _ e1 uncastSeq e2 -> do
      forM_ castSeq $ \(binder, term') -> do
        ensureTemplateResolvedInBinder h binder
        ensureTemplateResolved h m term'
      ensureTemplateResolved h m e1
      forM_ uncastSeq $ \(binder, term') -> do
        ensureTemplateResolvedInBinder h binder
        ensureTemplateResolved h m term'
      ensureTemplateResolved h m e2
    _ :< TM.Let _ (_, _, t) e1 e2 -> do
      ensureTemplateResolved h m t
      ensureTemplateResolved h m e1
      ensureTemplateResolved h m e2
    _ :< TM.Prim prim ->
      ensureTemplateResolvedInPrim h prim
    m' :< TM.Magic magic ->
      case magic of
        M.LowMagic lowMagic ->
          traverse_ (ensureTemplateResolved h m') lowMagic
        M.GetTypeTag _ _ _ ->
          raiseError m' "get-type-tag can only be used in template"
        M.GetConsSize _ ->
          raiseError m' "get-cons-size can only be used in template"
        M.GetConstructorArgTypes {} ->
          raiseError m' "get-constructor-arg-types can only be used in template"
        M.CompileError _ ->
          raiseError m' "compile-error can only be used in template"
    _ :< TM.Void ->
      return ()
    _ :< TM.Resource _ _ unitType discarder copier typeTag -> do
      ensureTemplateResolved h m unitType
      ensureTemplateResolved h m discarder
      ensureTemplateResolved h m copier
      ensureTemplateResolved h m typeTag

ensureTemplateResolvedInBinder :: Handle -> BinderF TM.Term -> App ()
ensureTemplateResolvedInBinder h (m, _, t) =
  ensureTemplateResolved h m t

ensureTemplateResolvedInDecisionTree ::
  Handle ->
  DT.DecisionTree TM.Term ->
  App ()
ensureTemplateResolvedInDecisionTree h tree =
  case tree of
    DT.Leaf _ letSeq e -> do
      forM_ letSeq $ \(binder, term') -> do
        ensureTemplateResolvedInBinder h binder
        ensureTemplateResolved h (getHint e) term'
      ensureTemplateResolved h (getHint e) e
    DT.Unreachable ->
      return ()
    DT.Switch (_, cursor) (fallbackTree, caseList) -> do
      ensureTemplateResolved h (getHint cursor) cursor
      ensureTemplateResolvedInDecisionTree h fallbackTree
      forM_ caseList $ \decisionCase ->
        case decisionCase of
          DT.LiteralCase _ _ cont ->
            ensureTemplateResolvedInDecisionTree h cont
          DT.ConsCase (DT.ConsCaseRecord {..}) -> do
            forM_ dataArgs $ \(term', typ) -> do
              ensureTemplateResolved h (getHint term') term'
              ensureTemplateResolved h (getHint typ) typ
            forM_ consArgs $ \(_, _, typ) ->
              ensureTemplateResolved h (getHint typ) typ
            ensureTemplateResolvedInDecisionTree h cont
  where
    getHint (hintValue :< _) = hintValue

ensureTemplateResolvedInPrim :: Handle -> P.Prim TM.Term -> App ()
ensureTemplateResolvedInPrim h prim =
  case prim of
    P.Type _ ->
      return ()
    P.Value pv ->
      case pv of
        PV.Int intType _ _ ->
          ensureTemplateResolved h (getHint intType) intType
        PV.Float floatType _ _ ->
          ensureTemplateResolved h (getHint floatType) floatType
        PV.Op {} ->
          return ()
        PV.StaticText {} ->
          return ()
        PV.Rune {} ->
          return ()
  where
    getHint (hintValue :< _) = hintValue
