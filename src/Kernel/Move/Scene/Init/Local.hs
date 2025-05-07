module Kernel.Move.Scene.Init.Local
  ( Handle (..),
    new,
  )
where

import Control.Monad.IO.Class
import Error.Rule.EIO (EIO)
import Kernel.Move.Context.Locator qualified as Locator
import Kernel.Move.Context.RawImportSummary qualified as RawImportSummary
import Kernel.Move.Context.SymLoc qualified as SymLoc
import Kernel.Move.Context.Tag qualified as Tag
import Kernel.Move.Context.TopCandidate qualified as TopCandidate
import Kernel.Move.Scene.Elaborate.Handle.WeakDecl qualified as WeakDecl
import Kernel.Move.Scene.Init.Base qualified as Base
import Kernel.Move.Scene.Parse.Handle.Alias qualified as Alias
import Kernel.Move.Scene.Parse.Handle.PreDecl qualified as PreDecl
import Kernel.Move.Scene.Parse.Handle.Unused qualified as Unused
import Kernel.Rule.Source qualified as Source

data Handle = Handle
  { unusedHandle :: Unused.Handle,
    aliasHandle :: Alias.Handle,
    locatorHandle :: Locator.Handle,
    tagHandle :: Tag.Handle,
    rawImportSummaryHandle :: RawImportSummary.Handle,
    symLocHandle :: SymLoc.Handle,
    topCandidateHandle :: TopCandidate.Handle,
    preDeclHandle :: PreDecl.Handle,
    weakDeclHandle :: WeakDecl.Handle
  }

new :: Base.Handle -> Source.Source -> EIO Handle
new h source = do
  let envHandle = Base.envHandle h
  let antecedentHandle = Base.antecedentHandle h
  unusedHandle <- liftIO Unused.new
  tagHandle <- liftIO Tag.new
  locatorHandle <- Locator.new envHandle tagHandle source
  aliasHandle <- liftIO $ Alias.new antecedentHandle locatorHandle envHandle source
  rawImportSummaryHandle <- liftIO RawImportSummary.new
  symLocHandle <- liftIO SymLoc.new
  topCandidateHandle <- liftIO TopCandidate.new
  preDeclHandle <- liftIO PreDecl.new
  weakDeclHandle <- liftIO WeakDecl.new
  return $ Handle {..}
