module Kernel.Common.Move.CreateLocalHandle
  ( Handle (..),
    new,
  )
where

import Control.Monad.IO.Class
import Kernel.Common.Move.CreateGlobalHandle qualified as Global
import Kernel.Common.Move.Handle.Local.Locator qualified as Locator
import Kernel.Common.Move.Handle.Local.RawImportSummary qualified as RawImportSummary
import Kernel.Common.Move.Handle.Local.SymLoc qualified as SymLoc
import Kernel.Common.Move.Handle.Local.Tag qualified as Tag
import Kernel.Common.Move.Handle.Local.TopCandidate qualified as TopCandidate
import Kernel.Common.Rule.Handle.Local.Locator qualified as Locator
import Kernel.Common.Rule.Handle.Local.RawImportSummary qualified as RawImportSummary
import Kernel.Common.Rule.Handle.Local.SymLoc qualified as SymLoc
import Kernel.Common.Rule.Handle.Local.Tag qualified as Tag
import Kernel.Common.Rule.Handle.Local.TopCandidate qualified as TopCandidate
import Kernel.Common.Rule.Source qualified as Source
import Kernel.Elaborate.Move.Internal.Handle.WeakDecl qualified as WeakDecl
import Kernel.Parse.Move.Internal.Handle.Alias qualified as Alias
import Kernel.Parse.Move.Internal.Handle.PreDecl qualified as PreDecl
import Kernel.Parse.Move.Internal.Handle.Unused qualified as Unused
import Library.Error.Rule.EIO (EIO)

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

new :: Global.Handle -> Source.Source -> EIO Handle
new h source = do
  let envHandle = Global.envHandle h
  let antecedentHandle = Global.antecedentHandle h
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
