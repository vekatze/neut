module Kernel.Elaborate.Internal.Handle.Elaborate
  ( Handle (..),
    new,
    reduceType,
    fillType,
    inline,
    inlineBinder,
  )
where

import App.App (App)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromMaybe)
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
import Kernel.Elaborate.Internal.Handle.Constraint qualified as Constraint
import Kernel.Elaborate.Internal.Handle.Def qualified as Definition
import Kernel.Elaborate.Internal.Handle.Hole qualified as Hole
import Kernel.Elaborate.Internal.Handle.LocalLogs qualified as LocalLogs
import Kernel.Elaborate.Internal.Handle.TypeDef qualified as TypeDef
import Kernel.Elaborate.Internal.Handle.WeakDecl qualified as WeakDecl
import Kernel.Elaborate.Internal.Handle.WeakDef qualified as WeakDef
import Kernel.Elaborate.Internal.Handle.WeakType qualified as WeakType
import Kernel.Elaborate.Internal.Handle.WeakTypeDef qualified as WeakTypeDef
import Kernel.Elaborate.Internal.WeakTerm.Fill qualified as Fill
import Kernel.Elaborate.TypeHoleSubst qualified as THS
import Language.Common.Binder
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
    typeDefHandle :: TypeDef.Handle,
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
  weakTypeHandle <- WeakType.new
  let varEnv = []
  let currentStep = 0
  return $ Handle {..}

reduceType :: Handle -> WT.WeakType -> App WT.WeakType
reduceType h t = do
  reduceHandle <- liftIO $ Reduce.new (substHandle h) (WT.metaOfType t) (inlineLimit h)
  Reduce.reduceType reduceHandle t

fillType :: Handle -> THS.TypeHoleSubst -> WT.WeakType -> App WT.WeakType
fillType h sub t = do
  reduceHandle <- liftIO $ Reduce.new (substHandle h) (WT.metaOfType t) (inlineLimit h)
  let substHandle = Subst.new (Global.gensymHandle (globalHandle h))
  let fillHandle = Fill.new substHandle reduceHandle
  Fill.fillType fillHandle sub t

inline :: Handle -> Hint -> TM.Term -> App TM.Term
inline h m e = do
  dmap <- liftIO $ Definition.get' (defHandle h)
  typeDefMap <- liftIO $ TypeDef.get' (typeDefHandle h)
  inlineHandle <- liftIO $ Inline.new (gensymHandle h) dmap typeDefMap m (inlineLimit h)
  Inline.inline inlineHandle e

inlineBinder :: Handle -> BinderF TM.Type -> App (BinderF TM.Type)
inlineBinder h (m, k, x, t) = do
  dmap <- liftIO $ Definition.get' (defHandle h)
  typeDefMap <- liftIO $ TypeDef.get' (typeDefHandle h)
  inlineHandle <- liftIO $ Inline.new (gensymHandle h) dmap typeDefMap m (inlineLimit h)
  t' <- Inline.inlineType inlineHandle t
  return (m, k, x, t')
