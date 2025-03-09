module Scene.Initialize
  ( initializeCompiler,
    initializeCompilerWithModule,
    initializeCompilerWithPath,
    initializeLogger,
    initializeForTarget,
    initializeForSource,
  )
where

import Context.Alias qualified as Alias
import Context.App
import Context.Color qualified as Color
import Context.Debug qualified as Debug
import Context.Decl qualified as Decl
import Context.Definition qualified as Definition
import Context.Env qualified as Env
import Context.Global qualified as Global
import Context.Locator qualified as Locator
import Context.RawImportSummary qualified as RawImportSummary
import Context.Remark qualified as Remark
import Context.SymLoc qualified as SymLoc
import Context.Tag qualified as Tag
import Context.TopCandidate qualified as TopCandidate
import Context.Type qualified as Type
import Context.Unravel qualified as Unravel
import Context.UnusedGlobalLocator qualified as UnusedGlobalLocator
import Context.UnusedLocalLocator qualified as UnusedLocalLocator
import Context.UnusedStaticFile qualified as UnusedStaticFile
import Context.UnusedVariable qualified as UnusedVariable
import Context.WeakDefinition qualified as WeakDefinition
import Entity.Config.Remark qualified as Remark
import Entity.Module
import Entity.Source qualified as Source
import Path
import Scene.Clarify qualified as Clarify
import Scene.Module.Reflect qualified as Module

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
  mainModule <- Module.fromCurrentPath
  initializeCompilerWithModule mainModule

initializeCompilerWithPath :: Path Abs File -> Remark.Config -> App ()
initializeCompilerWithPath path cfg = do
  initializeLogger cfg
  mainModule <- Module.fromFilePath path
  initializeCompilerWithModule mainModule

initializeCompilerWithModule :: Module -> App ()
initializeCompilerWithModule newModule = do
  Env.setMainModule newModule

initializeForTarget :: App ()
initializeForTarget = do
  Clarify.registerFoundationalTypes
  Unravel.initialize
  Remark.setGlobalRemarkList []
  Global.clearSourceNameMap
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
  Alias.initializeAliasMap
  Locator.initialize
  Tag.initialize
  RawImportSummary.initialize
  SymLoc.initialize
  TopCandidate.initialize
  Remark.setRemarkList []
  Decl.initialize
