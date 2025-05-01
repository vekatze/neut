module Move.Context.Elaborate
  ( Config (..),
    Handle (..),
    new,
    new',
  )
where

import Data.Maybe (fromMaybe)
import Move.Context.Clang qualified as Clang
import Move.Context.Debug qualified as Debug
import Move.Context.Definition qualified as Definition
import Move.Context.Env qualified as Env
import Move.Context.KeyArg qualified as KeyArg
import Move.Context.OptimizableData qualified as OptimizableData
import Move.Context.Path qualified as Path
import Move.Context.RawImportSummary qualified as RawImportSummary
import Move.Context.SymLoc qualified as SymLoc
import Move.Context.TopCandidate qualified as TopCandidate
import Move.Context.Type qualified as Type
import Move.Context.WeakDefinition qualified as WeakDefinition
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Elaborate.EnsureAffinity qualified as EnsureAffinity
import Move.Scene.Elaborate.Handle.Constraint qualified as Constraint
import Move.Scene.Elaborate.Handle.Hole qualified as Hole
import Move.Scene.Elaborate.Handle.WeakDecl qualified as WeakDecl
import Move.Scene.Elaborate.Handle.WeakType qualified as WeakType
import Move.Scene.Init.Base qualified as Base
import Move.Scene.Init.Local qualified as Local
import Move.Scene.Parse.Discern.Handle qualified as Discern
import Move.Scene.Term.Inline qualified as Inline
import Move.Scene.Term.Refresh qualified as Refresh
import Move.Scene.Term.Subst qualified as TermSubst
import Move.Scene.WeakTerm.Fill qualified as Fill
import Move.Scene.WeakTerm.Reduce qualified as Reduce
import Move.Scene.WeakTerm.Subst qualified as Subst
import Move.UI.Handle.GlobalRemark qualified as GlobalRemark
import Move.UI.Handle.LocalRemark qualified as LocalRemark
import Rule.Binder
import Rule.Const (defaultInlineLimit)
import Rule.Module (Module (moduleInlineLimit))
import Rule.Source
import Rule.WeakTerm qualified as WT

data Config = Config
  { _envHandle :: Env.Handle,
    _localRemarkHandle :: LocalRemark.Handle,
    _globalRemarkHandle :: GlobalRemark.Handle,
    _gensymHandle :: Gensym.Handle,
    _debugHandle :: Debug.Handle,
    _optDataHandle :: OptimizableData.Handle,
    _keyArgHandle :: KeyArg.Handle,
    _discernHandle :: Discern.Handle,
    _typeHandle :: Type.Handle,
    _clangHandle :: Clang.Handle,
    _rawImportSummaryHandle :: RawImportSummary.Handle,
    _symLocHandle :: SymLoc.Handle,
    _pathHandle :: Path.Handle,
    _topCandidateHandle :: TopCandidate.Handle,
    _weakDeclHandle :: WeakDecl.Handle,
    _weakDefHandle :: WeakDefinition.Handle,
    _defHandle :: Definition.Handle
  }

data Handle
  = Handle
  { envHandle :: Env.Handle,
    reduceHandle :: Reduce.Handle,
    weakDefHandle :: WeakDefinition.Handle,
    constraintHandle :: Constraint.Handle,
    holeHandle :: Hole.Handle,
    substHandle :: Subst.Handle,
    typeHandle :: Type.Handle,
    weakDeclHandle :: WeakDecl.Handle,
    defHandle :: Definition.Handle,
    gensymHandle :: Gensym.Handle,
    keyArgHandle :: KeyArg.Handle,
    localRemarkHandle :: LocalRemark.Handle,
    inlineHandle :: Inline.Handle,
    affHandle :: EnsureAffinity.Handle,
    pathHandle :: Path.Handle,
    symLocHandle :: SymLoc.Handle,
    topCandidateHandle :: TopCandidate.Handle,
    rawImportSummaryHandle :: RawImportSummary.Handle,
    globalRemarkHandle :: GlobalRemark.Handle,
    weakTypeHandle :: WeakType.Handle,
    discernHandle :: Discern.Handle,
    optDataHandle :: OptimizableData.Handle,
    fillHandle :: Fill.Handle,
    currentSource :: Source,
    inlineLimit :: Int,
    currentStep :: Int,
    varEnv :: BoundVarEnv
  }

type BoundVarEnv = [BinderF WT.WeakTerm]

new :: Config -> Source -> IO Handle
new cfg currentSource = do
  let envHandle = _envHandle cfg
  let gensymHandle = _gensymHandle cfg
  let optDataHandle = _optDataHandle cfg
  let keyArgHandle = _keyArgHandle cfg
  let discernHandle = _discernHandle cfg
  let typeHandle = _typeHandle cfg
  let rawImportSummaryHandle = _rawImportSummaryHandle cfg
  let symLocHandle = _symLocHandle cfg
  let pathHandle = _pathHandle cfg
  let topCandidateHandle = _topCandidateHandle cfg
  let localRemarkHandle = _localRemarkHandle cfg
  let globalRemarkHandle = _globalRemarkHandle cfg
  let weakDeclHandle = _weakDeclHandle cfg
  let weakDefHandle = _weakDefHandle cfg
  let defHandle = _defHandle cfg
  let substHandle = Subst.new gensymHandle
  let inlineLimit = fromMaybe defaultInlineLimit $ moduleInlineLimit (sourceModule currentSource)
  let reduceHandle = Reduce.new substHandle inlineLimit
  let termSubstHandle = TermSubst.new gensymHandle
  let refreshHandle = Refresh.new gensymHandle
  let inlineHandle = Inline.new currentSource termSubstHandle refreshHandle defHandle
  let affHandle = EnsureAffinity.new reduceHandle substHandle typeHandle weakDefHandle optDataHandle
  let fillHandle = Fill.new substHandle reduceHandle
  constraintHandle <- Constraint.new
  holeHandle <- Hole.new
  weakTypeHandle <- WeakType.new
  let varEnv = []
  let currentStep = 0
  return $ Handle {..}

new' :: Base.Handle -> Local.Handle -> Source -> IO Handle
new' baseHandle@(Base.Handle {..}) localHandle@(Local.Handle {..}) currentSource = do
  let substHandle = Subst.new gensymHandle
  let inlineLimit = fromMaybe defaultInlineLimit $ moduleInlineLimit (sourceModule currentSource)
  let reduceHandle = Reduce.new substHandle inlineLimit
  let refreshHandle = Refresh.new gensymHandle
  let termSubstHandle = TermSubst.new gensymHandle
  let inlineHandle = Inline.new currentSource termSubstHandle refreshHandle defHandle
  let affHandle = EnsureAffinity.new reduceHandle substHandle typeHandle weakDefHandle optDataHandle
  let fillHandle = Fill.new substHandle reduceHandle
  constraintHandle <- Constraint.new
  holeHandle <- Hole.new
  weakTypeHandle <- WeakType.new
  let varEnv = []
  let currentStep = 0
  let discernHandle = Discern.new' baseHandle localHandle
  return $ Handle {..}
