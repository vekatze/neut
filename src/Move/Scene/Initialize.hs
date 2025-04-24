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
import Control.Monad.Reader (asks)
import Move.Context.Alias qualified as Alias
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Move.Context.Color qualified as Color
import Move.Context.Debug qualified as Debug
import Move.Context.Decl qualified as Decl
import Move.Context.Definition qualified as Definition
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Context.Global qualified as Global
import Move.Context.Locator qualified as Locator
import Move.Context.RawImportSummary qualified as RawImportSummary
import Move.Context.Remark qualified as Remark
import Move.Context.SymLoc qualified as SymLoc
import Move.Context.Tag qualified as Tag
import Move.Context.TopCandidate qualified as TopCandidate
import Move.Context.Type qualified as Type
import Move.Context.Unravel qualified as Unravel
import Move.Context.UnusedGlobalLocator qualified as UnusedGlobalLocator
import Move.Context.UnusedLocalLocator qualified as UnusedLocalLocator
import Move.Context.UnusedStaticFile qualified as UnusedStaticFile
import Move.Context.UnusedVariable qualified as UnusedVariable
import Move.Context.WeakDefinition qualified as WeakDefinition
import Move.Scene.Clarify qualified as Clarify
import Move.Scene.Module.Reflect qualified as Module
import Move.UI.Handle.GlobalRemark qualified as GlobalRemark
import Path
import Rule.Config.Remark qualified as Remark
import Rule.Module
import Rule.Source qualified as Source

initializeLogger :: Remark.Config -> App ()
initializeLogger cfg = do
  Color.setShouldColorizeStdout $ Remark.shouldColorize cfg
  Color.setShouldColorizeStderr $ Remark.shouldColorize cfg
  Remark.setEndOfEntry $ Remark.endOfEntry cfg
  Env.setSilentMode $ Remark.enableSilentMode cfg
  Debug.setDebugMode $ Remark.enableDebugMode cfg

initializeCompiler :: Remark.Config -> App ()
initializeCompiler cfg = do
  initializeLogger cfg
  h <- Module.new
  mainModule <- toApp $ Module.fromCurrentPath h
  initializeCompilerWithModule mainModule

initializeCompilerWithPath :: Path Abs File -> Remark.Config -> App ()
initializeCompilerWithPath path cfg = do
  initializeLogger cfg
  h <- Module.new
  mainModule <- toApp $ Module.fromFilePath h path
  initializeCompilerWithModule mainModule

initializeCompilerWithModule :: Module -> App ()
initializeCompilerWithModule newModule = do
  Env.setMainModule (MainModule newModule)

initializeForTarget :: App ()
initializeForTarget = do
  Clarify.registerFoundationalTypes
  Unravel.initialize
  h <- GlobalRemark.new
  liftIO $ GlobalRemark.set h []
  asks App.sourceNameMap >>= liftIO . Global.clearSourceNameMap
  WeakDefinition.initialize
  Definition.initialize
  Type.initialize

initializeForSource :: Source.Source -> App ()
initializeForSource source = do
  UnusedVariable.initialize
  UnusedGlobalLocator.initialize
  UnusedLocalLocator.initialize
  UnusedStaticFile.initialize
  Remark.initialize
  Global.initialize
  Env.setCurrentSource source
  Alias.new >>= toApp . Alias.initializeAliasMap
  Locator.new >>= toApp . Locator.initialize
  Tag.initialize
  RawImportSummary.initialize
  SymLoc.initialize
  TopCandidate.initialize
  Remark.setRemarkList []
  Decl.initialize
