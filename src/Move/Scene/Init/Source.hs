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
import Move.Context.Unused qualified as Unused
import Move.Scene.Elaborate.Handle.WeakDecl qualified as WeakDecl
import Move.UI.Handle.LocalRemark qualified as LocalRemark
import Rule.Source qualified as Source

data Handle = Handle
  { unusedHandle :: Unused.Handle,
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

new ::
  Unused.Handle ->
  LocalRemark.Handle ->
  Global.Handle ->
  Env.Handle ->
  Alias.Handle ->
  Locator.Handle ->
  Tag.Handle ->
  RawImportSummary.Handle ->
  SymLoc.Handle ->
  TopCandidate.Handle ->
  PreDecl.Handle ->
  WeakDecl.Handle ->
  App Handle
new unusedHandle localRemarkHandle globalHandle envHandle aliasHandle locatorHandle tagHandle rawImportSummaryHandle symLocHandle topCandidateHandle preDeclHandle weakDeclHandle = do
  return $ Handle {..}

initializeForSource :: Handle -> Source.Source -> EIO ()
initializeForSource h source = do
  liftIO $ Unused.initialize (unusedHandle h)
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
