module Move.Scene.Elaborate.Handle.Elaborate
  ( Handle (..),
    new,
    reduce,
    fill,
    inline,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromMaybe)
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Context.KeyArg qualified as KeyArg
import Move.Context.OptimizableData qualified as OptimizableData
import Move.Context.Path qualified as Path
import Move.Context.Platform qualified as Platform
import Move.Context.RawImportSummary qualified as RawImportSummary
import Move.Context.SymLoc qualified as SymLoc
import Move.Context.TopCandidate qualified as TopCandidate
import Move.Context.Type qualified as Type
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Elaborate.Handle.Constraint qualified as Constraint
import Move.Scene.Elaborate.Handle.Def qualified as Definition
import Move.Scene.Elaborate.Handle.Hole qualified as Hole
import Move.Scene.Elaborate.Handle.WeakDecl qualified as WeakDecl
import Move.Scene.Elaborate.Handle.WeakDef qualified as WeakDef
import Move.Scene.Elaborate.Handle.WeakType qualified as WeakType
import Move.Scene.Init.Base qualified as Base
import Move.Scene.Init.Local qualified as Local
import Move.Scene.Parse.Discern.Handle qualified as Discern
import Move.Scene.Term.Inline qualified as Inline
import Move.Scene.WeakTerm.Fill qualified as Fill
import Move.Scene.WeakTerm.Reduce qualified as Reduce
import Move.Scene.WeakTerm.Subst qualified as Subst
import Move.UI.Handle.GlobalRemark qualified as GlobalRemark
import Move.UI.Handle.LocalRemark qualified as LocalRemark
import Rule.Binder
import Rule.Const (defaultInlineLimit)
import Rule.Hint (Hint)
import Rule.HoleSubst (HoleSubst)
import Rule.Module (Module (moduleInlineLimit))
import Rule.Source
import Rule.Term qualified as TM
import Rule.WeakTerm qualified as WT

data Handle
  = Handle
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
    localRemarkHandle :: LocalRemark.Handle,
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
  reduceHandle <- liftIO $ Reduce.new (baseHandle h) (WT.metaOf e) (currentSource h)
  Reduce.reduce reduceHandle e

fill :: Handle -> HoleSubst -> WT.WeakTerm -> EIO WT.WeakTerm
fill h sub e = do
  reduceHandle <- liftIO $ Reduce.new (baseHandle h) (WT.metaOf e) (currentSource h)
  let substHandle = Subst.new (Base.gensymHandle (baseHandle h))
  let fillHandle = Fill.new substHandle reduceHandle
  Fill.fill fillHandle sub e

inline :: Handle -> Hint -> TM.Term -> EIO TM.Term
inline h m e = do
  inlineHandle <- liftIO $ Inline.new (baseHandle h) (currentSource h) m
  Inline.inline inlineHandle e
