module Kernel.Elaborate.Move.Internal.Handle.Elaborate
  ( Handle (..),
    new,
    reduce,
    fill,
    inline,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromMaybe)
import Error.Rule.EIO (EIO)
import Gensym.Rule.Handle qualified as Gensym
import Kernel.Common.Rule.Const (defaultInlineLimit)
import Kernel.Common.Rule.Module (Module (moduleInlineLimit))
import Kernel.Common.Rule.Source
import Kernel.Elaborate.Move.Internal.Handle.Constraint qualified as Constraint
import Kernel.Elaborate.Move.Internal.Handle.Def qualified as Definition
import Kernel.Elaborate.Move.Internal.Handle.Hole qualified as Hole
import Kernel.Elaborate.Move.Internal.Handle.LocalLogs qualified as LocalLogs
import Kernel.Elaborate.Move.Internal.Handle.WeakDecl qualified as WeakDecl
import Kernel.Elaborate.Move.Internal.Handle.WeakDef qualified as WeakDef
import Kernel.Elaborate.Move.Internal.Handle.WeakType qualified as WeakType
import Kernel.Elaborate.Move.Internal.WeakTerm.Fill qualified as Fill
import Kernel.Elaborate.Rule.HoleSubst (HoleSubst)
import Kernel.Move.Context.Env qualified as Env
import Kernel.Move.Context.GlobalRemark qualified as GlobalRemark
import Kernel.Move.Context.KeyArg qualified as KeyArg
import Kernel.Move.Context.OptimizableData qualified as OptimizableData
import Kernel.Move.Context.Path qualified as Path
import Kernel.Move.Context.Platform qualified as Platform
import Kernel.Move.Context.RawImportSummary qualified as RawImportSummary
import Kernel.Move.Context.SymLoc qualified as SymLoc
import Kernel.Move.Context.TopCandidate qualified as TopCandidate
import Kernel.Move.Context.Type qualified as Type
import Kernel.Move.Scene.Init.Global qualified as Global
import Kernel.Move.Scene.Init.Local qualified as Local
import Language.Common.Rule.Binder
import Language.Term.Move.Inline qualified as Inline
import Language.Term.Rule.Term qualified as TM
import Language.WeakTerm.Move.Reduce qualified as Reduce
import Language.WeakTerm.Move.Subst qualified as Subst
import Language.WeakTerm.Rule.WeakTerm qualified as WT
import Logger.Rule.Hint (Hint)

data Handle = Handle
  { globalHandle :: Global.Handle,
    envHandle :: Env.Handle,
    platformHandle :: Platform.Handle,
    weakDefHandle :: WeakDef.Handle,
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
    symLocHandle :: SymLoc.Handle,
    topCandidateHandle :: TopCandidate.Handle,
    rawImportSummaryHandle :: RawImportSummary.Handle,
    globalRemarkHandle :: GlobalRemark.Handle,
    weakTypeHandle :: WeakType.Handle,
    optDataHandle :: OptimizableData.Handle,
    currentSource :: Source,
    inlineLimit :: Int,
    currentStep :: Int,
    varEnv :: BoundVarEnv
  }

type BoundVarEnv = [BinderF WT.WeakTerm]

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

reduce :: Handle -> WT.WeakTerm -> EIO WT.WeakTerm
reduce h e = do
  reduceHandle <- liftIO $ Reduce.new (substHandle h) (WT.metaOf e) (inlineLimit h)
  Reduce.reduce reduceHandle e

fill :: Handle -> HoleSubst -> WT.WeakTerm -> EIO WT.WeakTerm
fill h sub e = do
  reduceHandle <- liftIO $ Reduce.new (substHandle h) (WT.metaOf e) (inlineLimit h)
  let substHandle = Subst.new (Global.gensymHandle (globalHandle h))
  let fillHandle = Fill.new substHandle reduceHandle
  Fill.fill fillHandle sub e

inline :: Handle -> Hint -> TM.Term -> EIO TM.Term
inline h m e = do
  dmap <- liftIO $ Definition.get' (defHandle h)
  inlineHandle <- liftIO $ Inline.new (gensymHandle h) dmap m (inlineLimit h)
  Inline.inline inlineHandle e
