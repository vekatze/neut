module Main.Move.Scene.Elaborate.Handle.Elaborate
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
import Language.Common.Rule.Binder
import Language.Term.Move.Inline qualified as Inline
import Language.Term.Rule.Term qualified as TM
import Language.WeakTerm.Move.Reduce qualified as Reduce
import Language.WeakTerm.Move.Subst qualified as Subst
import Language.WeakTerm.Rule.WeakTerm qualified as WT
import Logger.Rule.Hint (Hint)
import Main.Move.Context.Env qualified as Env
import Main.Move.Context.GlobalRemark qualified as GlobalRemark
import Main.Move.Context.KeyArg qualified as KeyArg
import Main.Move.Context.OptimizableData qualified as OptimizableData
import Main.Move.Context.Path qualified as Path
import Main.Move.Context.Platform qualified as Platform
import Main.Move.Context.RawImportSummary qualified as RawImportSummary
import Main.Move.Context.SymLoc qualified as SymLoc
import Main.Move.Context.TopCandidate qualified as TopCandidate
import Main.Move.Context.Type qualified as Type
import Main.Move.Scene.Elaborate.Handle.Constraint qualified as Constraint
import Main.Move.Scene.Elaborate.Handle.Def qualified as Definition
import Main.Move.Scene.Elaborate.Handle.Hole qualified as Hole
import Main.Move.Scene.Elaborate.Handle.LocalLogs qualified as LocalLogs
import Main.Move.Scene.Elaborate.Handle.WeakDecl qualified as WeakDecl
import Main.Move.Scene.Elaborate.Handle.WeakDef qualified as WeakDef
import Main.Move.Scene.Elaborate.Handle.WeakType qualified as WeakType
import Main.Move.Scene.Elaborate.WeakTerm.Fill qualified as Fill
import Main.Move.Scene.Init.Base qualified as Base
import Main.Move.Scene.Init.Local qualified as Local
import Main.Move.Scene.Parse.Discern.Handle qualified as Discern
import Main.Rule.Const (defaultInlineLimit)
import Main.Rule.HoleSubst (HoleSubst)
import Main.Rule.Module (Module (moduleInlineLimit))
import Main.Rule.Source

data Handle = Handle
  { baseHandle :: Base.Handle,
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
    discernHandle :: Discern.Handle,
    optDataHandle :: OptimizableData.Handle,
    currentSource :: Source,
    inlineLimit :: Int,
    currentStep :: Int,
    varEnv :: BoundVarEnv
  }

type BoundVarEnv = [BinderF WT.WeakTerm]

new :: Base.Handle -> Local.Handle -> Source -> IO Handle
new baseHandle@(Base.Handle {..}) localHandle@(Local.Handle {..}) currentSource = do
  localLogsHandle <- LocalLogs.new
  let substHandle = Subst.new gensymHandle
  let inlineLimit = fromMaybe defaultInlineLimit $ moduleInlineLimit (sourceModule currentSource)
  constraintHandle <- Constraint.new
  holeHandle <- Hole.new
  weakTypeHandle <- WeakType.new
  let varEnv = []
  let currentStep = 0
  let discernHandle = Discern.new baseHandle localHandle
  return $ Handle {..}

reduce :: Handle -> WT.WeakTerm -> EIO WT.WeakTerm
reduce h e = do
  reduceHandle <- liftIO $ Reduce.new (substHandle h) (WT.metaOf e) (inlineLimit h)
  Reduce.reduce reduceHandle e

fill :: Handle -> HoleSubst -> WT.WeakTerm -> EIO WT.WeakTerm
fill h sub e = do
  reduceHandle <- liftIO $ Reduce.new (substHandle h) (WT.metaOf e) (inlineLimit h)
  let substHandle = Subst.new (Base.gensymHandle (baseHandle h))
  let fillHandle = Fill.new substHandle reduceHandle
  Fill.fill fillHandle sub e

inline :: Handle -> Hint -> TM.Term -> EIO TM.Term
inline h m e = do
  dmap <- liftIO $ Definition.get' (defHandle h)
  inlineHandle <- liftIO $ Inline.new (gensymHandle h) dmap m (inlineLimit h)
  Inline.inline inlineHandle e
