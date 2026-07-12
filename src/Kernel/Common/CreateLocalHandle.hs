module Kernel.Common.CreateLocalHandle
  ( Handle (..),
    new,
  )
where

import App.App (App)
import Control.Monad.IO.Class
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Local.Locator qualified as Locator
import Kernel.Common.Handle.Global.ModulePath qualified as ModulePath
import Kernel.Common.Handle.Local.RawImportSummary qualified as RawImportSummary
import Kernel.Common.Handle.Local.SymLoc qualified as SymLoc
import Kernel.Common.Handle.Local.Tag qualified as Tag
import Kernel.Common.Handle.Local.TopCandidate qualified as TopCandidate
import Kernel.Common.Source qualified as Source
import Kernel.Common.Source.ShiftToLatest qualified as STL
import Kernel.Elaborate.Internal.Handle.WeakDecl qualified as WeakDecl
import Kernel.Parse.Internal.Handle.Alias qualified as Alias
import Kernel.Parse.Internal.Handle.PreDecl qualified as PreDecl
import Kernel.Parse.Internal.Handle.Unused qualified as Unused
import Kernel.Parse.Internal.Handle.UsedTopLevelName qualified as UsedTopLevelName

data Handle = Handle
  { unusedHandle :: Unused.Handle,
    usedTopLevelNameHandle :: UsedTopLevelName.Handle,
    aliasHandle :: Alias.Handle,
    locatorHandle :: Locator.Handle,
    tagHandle :: Tag.Handle,
    rawImportSummaryHandle :: RawImportSummary.Handle,
    symLocHandle :: SymLoc.Handle,
    topCandidateHandle :: TopCandidate.Handle,
    preDeclHandle :: PreDecl.Handle,
    weakDeclHandle :: WeakDecl.Handle
  }

new :: Global.Handle -> Source.Source -> App Handle
new h source = do
  let envHandle = Global.envHandle h
  let antecedentHandle = Global.antecedentHandle h
  let shiftToLatestHandle = STL.new antecedentHandle
  unusedHandle <- liftIO Unused.new
  usedTopLevelNameHandle <- liftIO UsedTopLevelName.new
  tagHandle <- liftIO Tag.new
  modulePathMap <- liftIO $ ModulePath.get $ Global.modulePathHandle h
  locatorHandle <- Locator.new envHandle tagHandle modulePathMap source
  aliasHandle <- liftIO $ Alias.new shiftToLatestHandle locatorHandle envHandle (Global.moduleHandle h) source
  rawImportSummaryHandle <- liftIO RawImportSummary.new
  symLocHandle <- liftIO SymLoc.new
  topCandidateHandle <- liftIO TopCandidate.new
  preDeclHandle <- liftIO PreDecl.new
  weakDeclHandle <- liftIO WeakDecl.new
  return $ Handle {..}
