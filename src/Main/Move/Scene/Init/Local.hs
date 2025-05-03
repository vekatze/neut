module Main.Move.Scene.Init.Local
  ( Handle (..),
    new,
  )
where

import Control.Monad.IO.Class
import Main.Move.Context.EIO (EIO)
import Main.Move.Context.Locator qualified as Locator
import Main.Move.Context.RawImportSummary qualified as RawImportSummary
import Main.Move.Context.SymLoc qualified as SymLoc
import Main.Move.Context.Tag qualified as Tag
import Main.Move.Context.TopCandidate qualified as TopCandidate
import Main.Move.Scene.Elaborate.Handle.WeakDecl qualified as WeakDecl
import Main.Move.Scene.Init.Base qualified as Base
import Main.Move.Scene.Parse.Handle.Alias qualified as Alias
import Main.Move.Scene.Parse.Handle.Global qualified as Global
import Main.Move.Scene.Parse.Handle.PreDecl qualified as PreDecl
import Main.Move.Scene.Parse.Handle.Unused qualified as Unused
import Main.Move.UI.Handle.LocalRemark qualified as LocalRemark
import Main.Rule.Source qualified as Source

data Handle = Handle
  { unusedHandle :: Unused.Handle,
    localRemarkHandle :: LocalRemark.Handle,
    globalHandle :: Global.Handle,
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
  localRemarkHandle <- liftIO LocalRemark.new
  tagHandle <- liftIO Tag.new
  locatorHandle <- Locator.new envHandle tagHandle source
  aliasHandle <- liftIO $ Alias.new antecedentHandle locatorHandle envHandle source
  rawImportSummaryHandle <- liftIO RawImportSummary.new
  symLocHandle <- liftIO SymLoc.new
  topCandidateHandle <- liftIO TopCandidate.new
  preDeclHandle <- liftIO PreDecl.new
  weakDeclHandle <- liftIO WeakDecl.new
  globalHandle <- liftIO $ Global.new h locatorHandle unusedHandle tagHandle
  return $ Handle {..}
