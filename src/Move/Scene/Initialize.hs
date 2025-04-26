module Move.Scene.Initialize
  ( initializeCompiler,
    initializeCompilerWithModule,
    initializeCompilerWithPath,
    initializeLogger,
    initializeForTarget,
    initializeForSource,
  )
where

import Control.Monad.IO.Class
import Move.Console.Report qualified as Report
import Move.Context.Alias qualified as Alias
import Move.Context.Antecedent qualified as Antecedent
import Move.Context.App
import Move.Context.Color qualified as Color
import Move.Context.Debug qualified as Debug
import Move.Context.Definition qualified as Definition
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Context.Global qualified as Global
import Move.Context.Locator qualified as Locator
import Move.Context.PreDecl qualified as PreDecl
import Move.Context.RawImportSummary qualified as RawImportSummary
import Move.Context.SymLoc qualified as SymLoc
import Move.Context.Tag qualified as Tag
import Move.Context.TopCandidate qualified as TopCandidate
import Move.Context.Type qualified as Type
import Move.Context.UnusedGlobalLocator qualified as UnusedGlobalLocator
import Move.Context.UnusedLocalLocator qualified as UnusedLocalLocator
import Move.Context.UnusedStaticFile qualified as UnusedStaticFile
import Move.Context.UnusedVariable qualified as UnusedVariable
import Move.Context.WeakDefinition qualified as WeakDefinition
import Move.Scene.Clarify qualified as Clarify
import Move.Scene.Elaborate.Handle.WeakDecl qualified as WeakDecl
import Move.Scene.Module.Reflect qualified as ModuleReflect
import Move.Scene.Unravel qualified as Unravel
import Move.UI.Handle.GlobalRemark qualified as GlobalRemark
import Move.UI.Handle.LocalRemark qualified as LocalRemark
import Path
import Rule.Config.Remark qualified as Remark
import Rule.Module
import Rule.Source qualified as Source

initializeLogger :: Remark.Config -> App ()
initializeLogger cfg = do
  h <- Color.new
  liftIO $ Color.setShouldColorizeStdout h $ Remark.shouldColorize cfg
  liftIO $ Color.setShouldColorizeStderr h $ Remark.shouldColorize cfg
  hr <- Report.new
  liftIO $ Report.setEndOfEntry hr $ Remark.endOfEntry cfg
  Env.setSilentMode $ Remark.enableSilentMode cfg
  hd <- Debug.new
  liftIO $ Debug.setDebugMode hd $ Remark.enableDebugMode cfg

initializeCompiler :: ModuleReflect.Handle -> Remark.Config -> App ()
initializeCompiler h cfg = do
  initializeLogger cfg
  mainModule <- toApp $ ModuleReflect.fromCurrentPath h
  initializeCompilerWithModule mainModule

initializeCompilerWithPath :: ModuleReflect.Handle -> Path Abs File -> Remark.Config -> App ()
initializeCompilerWithPath h path cfg = do
  initializeLogger cfg
  mainModule <- toApp $ ModuleReflect.fromFilePath h path
  initializeCompilerWithModule mainModule

initializeCompilerWithModule :: Module -> App ()
initializeCompilerWithModule newModule = do
  Env.setMainModule (MainModule newModule)

initializeForTarget :: App ()
initializeForTarget = do
  hc <- Clarify.new
  liftIO $ Clarify.registerFoundationalTypes hc
  Unravel.initialize
  Antecedent.initialize
  h <- GlobalRemark.new
  liftIO $ GlobalRemark.set h []
  WeakDefinition.initialize
  Definition.initialize
  Type.initialize

initializeForSource :: Source.Source -> App ()
initializeForSource source = do
  UnusedVariable.new >>= liftIO . UnusedVariable.initialize
  UnusedGlobalLocator.new >>= liftIO . UnusedGlobalLocator.initialize
  UnusedLocalLocator.new >>= liftIO . UnusedLocalLocator.initialize
  UnusedStaticFile.initialize
  LocalRemark.initialize
  Global.initialize
  Env.setCurrentSource source
  Alias.new >>= toApp . Alias.initializeAliasMap
  Locator.new >>= toApp . Locator.initialize
  Tag.initialize
  RawImportSummary.initialize
  SymLoc.initialize
  TopCandidate.initialize
  PreDecl.initialize
  WeakDecl.initialize
