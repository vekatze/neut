module Move.Scene.Init.Source
  ( Handle,
    new,
    initializeForSource,
  )
where

import Control.Monad.IO.Class
import Move.Context.Alias qualified as Alias
import Move.Context.App
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Context.Global qualified as Global
import Move.Context.Locator qualified as Locator
import Move.Context.PreDecl qualified as PreDecl
import Move.Context.RawImportSummary qualified as RawImportSummary
import Move.Context.SymLoc qualified as SymLoc
import Move.Context.Tag qualified as Tag
import Move.Context.TopCandidate qualified as TopCandidate
import Move.Context.UnusedGlobalLocator qualified as UnusedGlobalLocator
import Move.Context.UnusedLocalLocator qualified as UnusedLocalLocator
import Move.Context.UnusedStaticFile qualified as UnusedStaticFile
import Move.Context.UnusedVariable qualified as UnusedVariable
import Move.Scene.Elaborate.Handle.WeakDecl qualified as WeakDecl
import Move.UI.Handle.LocalRemark qualified as LocalRemark
import Rule.Source qualified as Source

data Handle = Handle
  { unusedVariableHandle :: UnusedVariable.Handle,
    unusedGlobalLocatorHandle :: UnusedGlobalLocator.Handle,
    unusedLocalLocatorHandle :: UnusedLocalLocator.Handle,
    unusedStaticFileHandle :: UnusedStaticFile.Handle,
    localRemarkHandle :: LocalRemark.Handle,
    globalHandle :: Global.Handle,
    envHandle :: Env.Handle,
    aliasHandle :: Alias.Handle,
    locatorHandle :: Locator.Handle,
    tagHandle :: Tag.Handle,
    rawImportSummaryHandle :: RawImportSummary.Handle,
    symLocHandle :: SymLoc.Handle,
    topCandidateHandle :: TopCandidate.Handle,
    preDeclHandle :: PreDecl.Handle,
    weakDeclHandle :: WeakDecl.Handle
  }

new :: Env.Handle -> Locator.Handle -> App Handle
new envHandle locatorHandle = do
  unusedVariableHandle <- UnusedVariable.new
  unusedGlobalLocatorHandle <- UnusedGlobalLocator.new
  unusedLocalLocatorHandle <- UnusedLocalLocator.new
  unusedStaticFileHandle <- UnusedStaticFile.new
  localRemarkHandle <- LocalRemark.new
  globalHandle <- Global.new envHandle
  aliasHandle <- Alias.new envHandle
  tagHandle <- Tag.new
  rawImportSummaryHandle <- RawImportSummary.new
  symLocHandle <- SymLoc.new
  topCandidateHandle <- TopCandidate.new
  preDeclHandle <- PreDecl.new
  weakDeclHandle <- WeakDecl.new
  return $ Handle {..}

initializeForSource :: Handle -> Source.Source -> EIO ()
initializeForSource h source = do
  liftIO $ UnusedVariable.initialize (unusedVariableHandle h)
  liftIO $ UnusedGlobalLocator.initialize (unusedGlobalLocatorHandle h)
  liftIO $ UnusedLocalLocator.initialize (unusedLocalLocatorHandle h)
  liftIO $ UnusedStaticFile.initialize (unusedStaticFileHandle h)
  liftIO $ LocalRemark.initialize (localRemarkHandle h)
  liftIO $ Global.initialize (globalHandle h)
  liftIO $ Env.setCurrentSource (envHandle h) source
  Alias.initializeAliasMap (aliasHandle h)
  Locator.initialize (locatorHandle h)
  liftIO $ Tag.initialize (tagHandle h)
  liftIO $ RawImportSummary.initialize (rawImportSummaryHandle h)
  liftIO $ SymLoc.initialize (symLocHandle h)
  liftIO $ TopCandidate.initialize (topCandidateHandle h)
  liftIO $ PreDecl.initialize (preDeclHandle h)
  WeakDecl.initialize (weakDeclHandle h)
