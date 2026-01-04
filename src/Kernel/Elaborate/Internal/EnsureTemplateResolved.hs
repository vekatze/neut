module Kernel.Elaborate.Internal.EnsureTemplateResolved
  ( ensureTemplateResolved,
  )
where

import App.App (App)
import App.Run (raiseError)
import Control.Comonad.Cofree
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Kernel.Elaborate.Internal.Handle.Def qualified as Definition
import Kernel.Elaborate.Internal.Handle.Elaborate (Handle, defHandle)
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Attr.Lam qualified as AttrL
import Language.Common.BaseLowType qualified as BLT
import Language.Common.Binder
import Language.Common.DecisionTree qualified as DT
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.LamKind qualified as LK
import Language.Common.LowMagic qualified as LM
import Language.Common.Magic qualified as M
import Language.Term.PrimValue qualified as PV
import Language.Term.Term qualified as TM
import Logger.Hint

-- Ensure that all templates are fully resolved (no template functions or primitives remain).
ensureTemplateResolved :: Handle -> Hint -> TM.Term -> App ()
ensureTemplateResolved h m term =
  case term of
    _ :< TM.Var {} ->
      return ()
    m' :< TM.VarGlobal _ dd -> do
      -- Check if this is a call to a template function.
      isTemplate <- liftIO $ Definition.isTemplate' (defHandle h) dd
      when isTemplate $ do
        raiseError m' $ "template function `" <> DD.reify dd <> "` cannot be called from non-template code"
    _ :< TM.PiIntro (AttrL.Attr {lamKind}) impArgs defaultArgs expArgs e -> do
      mapM_ (ensureTemplateResolvedInBinder h) impArgs
      forM_ defaultArgs $ \(binder, value) -> do
        ensureTemplateResolvedInBinder h binder
        ensureTemplateResolved h m value
      mapM_ (ensureTemplateResolvedInBinder h) expArgs
      case lamKind of
        LK.Fix _ xt ->
          ensureTemplateResolvedInBinder h xt
        LK.Normal _ codType ->
          ensureTemplateResolvedType h m codType
      ensureTemplateResolved h m e
    _ :< TM.PiElim _ e impArgs expArgs -> do
      ensureTemplateResolved h m e
      mapM_ (ensureTemplateResolvedType h m) impArgs
      mapM_ (ensureTemplateResolved h m) expArgs
    _ :< TM.DataIntro _ _ dataArgs consArgs -> do
      mapM_ (ensureTemplateResolvedType h m) dataArgs
      mapM_ (ensureTemplateResolved h m) consArgs
    _ :< TM.DataElim _ oets decisionTree -> do
      forM_ oets $ \(_, e, t) -> do
        ensureTemplateResolved h m e
        ensureTemplateResolvedType h m t
      ensureTemplateResolvedInDecisionTree h decisionTree
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
    _ :< TM.CodeIntro e ->
      ensureTemplateResolved h m e
    _ :< TM.CodeElim e ->
      ensureTemplateResolved h m e
    _ :< TM.TauIntro ty ->
      ensureTemplateResolvedType h m ty
    _ :< TM.TauElim _ e1 e2 -> do
      ensureTemplateResolved h m e1
      ensureTemplateResolved h m e2
    _ :< TM.Let _ (_, _, t) e1 e2 -> do
      ensureTemplateResolvedType h m t
      ensureTemplateResolved h m e1
      ensureTemplateResolved h m e2
    _ :< TM.Prim prim ->
      ensureTemplateResolvedInPrim h prim
    m' :< TM.Magic magic ->
      case magic of
        M.LowMagic lowMagic ->
          ensureTemplateResolvedInLowMagic h m' lowMagic
        M.GetTypeTag _ _ _ ->
          raiseError m' "get-type-tag can only be used in template"
        M.GetConsSize _ ->
          raiseError m' "get-cons-size can only be used in template"
        M.GetConstructorArgTypes {} ->
          raiseError m' "get-constructor-arg-types can only be used in template"
        M.CompileError _ ->
          raiseError m' "compile-error can only be used in template"

ensureTemplateResolvedInBinder :: Handle -> BinderF TM.Type -> App ()
ensureTemplateResolvedInBinder h (m, _, t) =
  ensureTemplateResolvedType h m t

ensureTemplateResolvedInDecisionTree ::
  Handle ->
  DT.DecisionTree TM.Type TM.Term ->
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
      ensureTemplateResolvedType h (getHint cursor) cursor
      ensureTemplateResolvedInDecisionTree h fallbackTree
      forM_ caseList $ \decisionCase ->
        case decisionCase of
          DT.LiteralCase _ _ cont ->
            ensureTemplateResolvedInDecisionTree h cont
          DT.ConsCase (DT.ConsCaseRecord {..}) -> do
            forM_ dataArgs $ \(term', typ) -> do
              ensureTemplateResolvedType h (getHint term') term'
              ensureTemplateResolvedType h (getHint typ) typ
            forM_ consArgs $ \(_, _, typ) ->
              ensureTemplateResolvedType h (getHint typ) typ
            ensureTemplateResolvedInDecisionTree h cont
  where
    getHint (hintValue :< _) = hintValue

ensureTemplateResolvedInPrim :: Handle -> PV.PrimValue TM.Type -> App ()
ensureTemplateResolvedInPrim h prim =
  case prim of
    PV.Int intType _ _ ->
      ensureTemplateResolvedType h (getHint intType) intType
    PV.Float floatType _ _ ->
      ensureTemplateResolvedType h (getHint floatType) floatType
    PV.Op {} ->
      return ()
    PV.StaticText {} ->
      return ()
    PV.Rune {} ->
      return ()
  where
    getHint (hintValue :< _) = hintValue

ensureTemplateResolvedInLowMagic :: Handle -> Hint -> LM.LowMagic BLT.BaseLowType TM.Type TM.Term -> App ()
ensureTemplateResolvedInLowMagic h m lowMagic =
  case lowMagic of
    LM.Cast from to value -> do
      ensureTemplateResolvedType h m from
      ensureTemplateResolvedType h m to
      ensureTemplateResolved h m value
    LM.Store _ _ value pointer -> do
      ensureTemplateResolved h m value
      ensureTemplateResolved h m pointer
    LM.Load _ pointer -> do
      ensureTemplateResolved h m pointer
    LM.Alloca _ size -> do
      ensureTemplateResolved h m size
    LM.External _ _ _ args varArgs -> do
      mapM_ (ensureTemplateResolved h m) args
      forM_ varArgs $ \(arg, _) ->
        ensureTemplateResolved h m arg
    LM.Global {} ->
      return ()
    LM.OpaqueValue e ->
      ensureTemplateResolved h m e
    LM.CallType func arg1 arg2 -> do
      ensureTemplateResolved h m func
      ensureTemplateResolved h m arg1
      ensureTemplateResolved h m arg2

ensureTemplateResolvedType :: Handle -> Hint -> TM.Type -> App ()
ensureTemplateResolvedType h m ty =
  case ty of
    _ :< TM.Tau ->
      return ()
    _ :< TM.TVar {} ->
      return ()
    _ :< TM.TVarGlobal {} ->
      return ()
    _ :< TM.TyApp t args -> do
      ensureTemplateResolvedType h m t
      mapM_ (ensureTemplateResolvedType h m) args
    _ :< TM.Pi _ impArgs defaultArgs expArgs cod -> do
      mapM_ (ensureTemplateResolvedInBinder h) impArgs
      forM_ defaultArgs $ \(binder, value) -> do
        ensureTemplateResolvedInBinder h binder
        ensureTemplateResolved h m value
      mapM_ (ensureTemplateResolvedInBinder h) expArgs
      ensureTemplateResolvedType h m cod
    _ :< TM.Data attr _ args -> do
      mapM_ (ensureTemplateResolvedType h m) args
      ensureTemplateResolvedInAttrData h attr
    _ :< TM.Box t ->
      ensureTemplateResolvedType h m t
    _ :< TM.BoxNoema t ->
      ensureTemplateResolvedType h m t
    _ :< TM.Code t ->
      ensureTemplateResolvedType h m t
    _ :< TM.PrimType {} ->
      return ()
    _ :< TM.Void ->
      return ()
    _ :< TM.Resource _ _ unitType discarder copier typeTag -> do
      ensureTemplateResolvedType h m unitType
      ensureTemplateResolved h m discarder
      ensureTemplateResolved h m copier
      ensureTemplateResolved h m typeTag

ensureTemplateResolvedInAttrData :: Handle -> AttrD.Attr name (BinderF TM.Type) -> App ()
ensureTemplateResolvedInAttrData h attr =
  forM_ (AttrD.consNameList attr) $ \(_, binders, _) ->
    mapM_ (ensureTemplateResolvedInBinder h) binders
