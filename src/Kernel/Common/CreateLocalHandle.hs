module Kernel.Common.CreateLocalHandle
  ( Handle (..),
    new,
  )
where

import Control.Monad.IO.Class
import Error.EIO (EIO)
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Local.Locator qualified as Locator
import Kernel.Common.Handle.Local.RawImportSummary qualified as RawImportSummary
import Kernel.Common.Handle.Local.SymLoc qualified as SymLoc
import Kernel.Common.Handle.Local.Tag qualified as Tag
import Kernel.Common.Handle.Local.TopCandidate qualified as TopCandidate
import Kernel.Common.Source qualified as Source
import Kernel.Elaborate.Internal.Handle.WeakDecl qualified as WeakDecl
import Kernel.Parse.Internal.Handle.Alias qualified as Alias
import Kernel.Parse.Internal.Handle.PreDecl qualified as PreDecl
import Kernel.Parse.Internal.Handle.Unused qualified as Unused

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
