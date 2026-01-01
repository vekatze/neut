module Kernel.Elaborate.Internal.Handle.Elaborate
  ( Handle (..),
    new,
    reduce,
    reduceType,
    fill,
    fillType,
    inline,
    inlineBinder,
  )
where

import App.App (App)
import Control.Comonad.Cofree
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashMap.Strict qualified as Map
import Data.IntMap qualified as IntMap
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Gensym.Handle qualified as Gensym
import Kernel.Common.Const (defaultInlineLimit)
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.CreateLocalHandle qualified as Local
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.GlobalRemark qualified as GlobalRemark
import Kernel.Common.Handle.Global.KeyArg qualified as KeyArg
import Kernel.Common.Handle.Global.OptimizableData qualified as OptimizableData
import Kernel.Common.Handle.Global.Path qualified as Path
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Handle.Global.Type qualified as Type
import Kernel.Common.Module (Module (moduleInlineLimit))
import Kernel.Common.Source
import Kernel.Elaborate.TypeHoleSubst qualified as THS
import Kernel.Elaborate.Internal.Handle.Constraint qualified as Constraint
import Kernel.Elaborate.Internal.Handle.Def qualified as Definition
import Kernel.Elaborate.Internal.Handle.Hole qualified as Hole
import Kernel.Elaborate.Internal.Handle.LocalLogs qualified as LocalLogs
import Kernel.Elaborate.Internal.Handle.WeakDecl qualified as WeakDecl
import Kernel.Elaborate.Internal.Handle.WeakDef qualified as WeakDef
import Kernel.Elaborate.Internal.Handle.WeakTypeDef qualified as WeakTypeDef
import Kernel.Elaborate.Internal.Handle.WeakType qualified as WeakType
import Kernel.Elaborate.Internal.WeakTerm.Fill qualified as Fill
import Language.Common.Attr.Data qualified as AttrD
import Language.Common.Binder
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Ident.Reify qualified as Ident
import Language.Term.Inline qualified as Inline
import Language.Term.Term qualified as TM
import Language.WeakTerm.Reduce qualified as Reduce
import Language.WeakTerm.Subst qualified as Subst
import Language.WeakTerm.WeakTerm qualified as WT
import Logger.Hint (Hint)

data Handle = Handle
  { globalHandle :: Global.Handle,
    envHandle :: Env.Handle,
    platformHandle :: Platform.Handle,
    weakDefHandle :: WeakDef.Handle,
    weakTypeDefHandle :: WeakTypeDef.Handle,
    constraintHandle :: Constraint.Handle,
    holeHandle :: Hole.Handle,
    substHandle :: Subst.Handle,
    typeHandle :: Type.Handle,
    weakDeclHandle :: WeakDecl.Handle,
    defHandle :: Definition.Handle,
    gensymHandle :: Gensym.Handle,
    keyArgHandle :: KeyArg.Handle,
    localLogsHandle :: LocalLogs.Handle,
    pathHandle :: Path.Handle,
    globalRemarkHandle :: GlobalRemark.Handle,
    weakTypeHandle :: WeakType.Handle,
    optDataHandle :: OptimizableData.Handle,
    currentSource :: Source,
    inlineLimit :: Int,
    currentStep :: Int,
    varEnv :: BoundVarEnv
  }

type BoundVarEnv = [BinderF WT.WeakType]

new :: Global.Handle -> Local.Handle -> Source -> IO Handle
new globalHandle@(Global.Handle {..}) (Local.Handle {..}) currentSource = do
  localLogsHandle <- LocalLogs.new
  let substHandle = Subst.new gensymHandle
  let inlineLimit = fromMaybe defaultInlineLimit $ moduleInlineLimit (sourceModule currentSource)
  constraintHandle <- Constraint.new
  holeHandle <- Hole.new
  weakTypeDefHandle <- WeakTypeDef.new
  weakTypeHandle <- WeakType.new
  let varEnv = []
  let currentStep = 0
  return $ Handle {..}

reduce :: Handle -> WT.WeakTerm -> App WT.WeakTerm
reduce h e = do
  reduceHandle <- liftIO $ Reduce.new (substHandle h) (WT.metaOf e) (inlineLimit h)
  Reduce.reduce reduceHandle e

reduceType :: Handle -> WT.WeakType -> App WT.WeakType
reduceType h t = do
  reduceHandle <- liftIO $ Reduce.new (substHandle h) (WT.metaOfType t) (inlineLimit h)
  Reduce.reduceType reduceHandle t >>= expandTypeDefs h

fill :: Handle -> THS.TypeHoleSubst -> WT.WeakTerm -> App WT.WeakTerm
fill h sub e = do
  reduceHandle <- liftIO $ Reduce.new (substHandle h) (WT.metaOf e) (inlineLimit h)
  let substHandle = Subst.new (Global.gensymHandle (globalHandle h))
  let fillHandle = Fill.new substHandle reduceHandle
  Fill.fill fillHandle sub e

fillType :: Handle -> THS.TypeHoleSubst -> WT.WeakType -> App WT.WeakType
fillType h sub t = do
  reduceHandle <- liftIO $ Reduce.new (substHandle h) (WT.metaOfType t) (inlineLimit h)
  let substHandle = Subst.new (Global.gensymHandle (globalHandle h))
  let fillHandle = Fill.new substHandle reduceHandle
  Fill.fillType fillHandle sub t

inline :: Handle -> Hint -> TM.Term -> App TM.Term
inline h m e = do
  dmap <- liftIO $ Definition.get' (defHandle h)
  inlineHandle <- liftIO $ Inline.new (gensymHandle h) dmap m (inlineLimit h)
  Inline.inline inlineHandle e

inlineBinder :: Handle -> BinderF TM.Type -> App (BinderF TM.Type)
inlineBinder h (m, x, t) = do
  dmap <- liftIO $ Definition.get' (defHandle h)
  inlineHandle <- liftIO $ Inline.new (gensymHandle h) dmap m (inlineLimit h)
  t' <- Inline.inlineType inlineHandle t
  return (m, x, t')

expandTypeDefs :: Handle -> WT.WeakType -> App WT.WeakType
expandTypeDefs h t = do
  defMap <- liftIO $ WeakTypeDef.read' (weakTypeDefHandle h)
  expandTypeDefs' defMap S.empty t

expandTypeDefs' :: Map.HashMap DD.DefiniteDescription WeakTypeDef.TypeDef -> S.Set DD.DefiniteDescription -> WT.WeakType -> App WT.WeakType
expandTypeDefs' defMap seen ty =
  case ty of
    _ :< WT.Tau ->
      return ty
    _ :< WT.TVar {} ->
      return ty
    _ :< WT.TVarGlobal _ name
      | Just def <- Map.lookup name defMap,
        name `S.notMember` seen,
        null (WeakTypeDef.typeDefBinders def) -> do
          expandTypeDefs' defMap (S.insert name seen) (WeakTypeDef.typeDefBody def)
      | otherwise ->
          return ty
    m :< WT.TyApp t args -> do
      t' <- expandTypeDefs' defMap seen t
      args' <- mapM (expandTypeDefs' defMap seen) args
      let (base, allArgs) = collectTyApp t' args'
      case base of
        _ :< WT.TVarGlobal _ name
          | Just def <- Map.lookup name defMap,
            name `S.notMember` seen,
            length allArgs == length (WeakTypeDef.typeDefBinders def) -> do
              let binderIds = map (\(_, x, _) -> Ident.toInt x) (WeakTypeDef.typeDefBinders def)
              let sub = IntMap.fromList $ zip binderIds (map Right allArgs)
              body' <- liftIO $ Subst.substTypeWith sub (WeakTypeDef.typeDefBody def)
              expandTypeDefs' defMap (S.insert name seen) body'
        _ ->
          return $ m :< WT.TyApp base allArgs
    m :< WT.Pi piKind impArgs defaultArgs expArgs cod -> do
      impArgs' <- mapM (expandTypeBinder defMap seen) impArgs
      defaultArgs' <- mapM (expandTypeDefaultArg defMap seen) defaultArgs
      expArgs' <- mapM (expandTypeBinder defMap seen) expArgs
      cod' <- expandTypeDefs' defMap seen cod
      return $ m :< WT.Pi piKind impArgs' defaultArgs' expArgs' cod'
    m :< WT.Data attr name es -> do
      es' <- mapM (expandTypeDefs' defMap seen) es
      attr' <- expandAttrData defMap seen attr
      return $ m :< WT.Data attr' name es'
    m :< WT.Box t -> do
      t' <- expandTypeDefs' defMap seen t
      return $ m :< WT.Box t'
    m :< WT.BoxNoema t -> do
      t' <- expandTypeDefs' defMap seen t
      return $ m :< WT.BoxNoema t'
    m :< WT.Code t -> do
      t' <- expandTypeDefs' defMap seen t
      return $ m :< WT.Code t'
    _ :< WT.PrimType {} ->
      return ty
    _ :< WT.Void ->
      return ty
    m :< WT.Resource dd resourceID unitType discarder copier typeTag -> do
      unitType' <- expandTypeDefs' defMap seen unitType
      return $ m :< WT.Resource dd resourceID unitType' discarder copier typeTag
    m :< WT.TypeHole hole args -> do
      args' <- mapM (expandTypeDefs' defMap seen) args
      return $ m :< WT.TypeHole hole args'

expandTypeBinder ::
  Map.HashMap DD.DefiniteDescription WeakTypeDef.TypeDef ->
  S.Set DD.DefiniteDescription ->
  BinderF WT.WeakType ->
  App (BinderF WT.WeakType)
expandTypeBinder defMap seen (m, x, t) = do
  t' <- expandTypeDefs' defMap seen t
  return (m, x, t')

expandTypeDefaultArg ::
  Map.HashMap DD.DefiniteDescription WeakTypeDef.TypeDef ->
  S.Set DD.DefiniteDescription ->
  (BinderF WT.WeakType, WT.WeakTerm) ->
  App (BinderF WT.WeakType, WT.WeakTerm)
expandTypeDefaultArg defMap seen (binder, value) = do
  binder' <- expandTypeBinder defMap seen binder
  return (binder', value)

expandAttrData ::
  Map.HashMap DD.DefiniteDescription WeakTypeDef.TypeDef ->
  S.Set DD.DefiniteDescription ->
  AttrD.Attr name (BinderF WT.WeakType) ->
  App (AttrD.Attr name (BinderF WT.WeakType))
expandAttrData defMap seen attr = do
  let consNameList = AttrD.consNameList attr
  consNameList' <- forM consNameList $ \(cn, binders, cl) -> do
    binders' <- mapM (expandTypeBinder defMap seen) binders
    return (cn, binders', cl)
  return $ attr {AttrD.consNameList = consNameList'}

collectTyApp :: WT.WeakType -> [WT.WeakType] -> (WT.WeakType, [WT.WeakType])
collectTyApp t args =
  case t of
    _ :< WT.TyApp t' args' ->
      collectTyApp t' (args' ++ args)
    _ ->
      (t, args)
