module Scene.Initialize
  ( initializeCompiler,
    initializeCompilerWithModule,
    initializeLogger,
    initializeForTarget,
    initializeForSource,
  )
where

import Context.Alias qualified as Alias
import Context.App
import Context.Decl qualified as Decl
import Context.Env qualified as Env
import Context.Global qualified as Global
import Context.LLVM qualified as LLVM
import Context.Locator qualified as Locator
import Context.Module qualified as Module
import Context.Remark qualified as Remark
import Context.Tag qualified as Tag
import Context.Unravel qualified as Unravel
import Context.UnusedVariable qualified as UnusedVariable
import Data.Maybe
import Entity.Config.Remark qualified as Remark
import Entity.Module
import Entity.Source qualified as Source
import Scene.Clarify qualified as Clarify
import Scene.Module.Reflect qualified as Module

initializeLogger :: Remark.Config -> App ()
initializeLogger cfg = do
  Remark.setEndOfEntry $ Remark.endOfEntry cfg
  Remark.setShouldColorize $ Remark.shouldColorize cfg

initializeCompiler :: Remark.Config -> Maybe String -> App ()
initializeCompiler cfg mClangOptString = do
  initializeLogger cfg
  mainModule <- Module.fromCurrentPath
  initializeCompilerWithModule mainModule mClangOptString

initializeCompilerWithModule :: Module -> Maybe String -> App ()
initializeCompilerWithModule newModule mClangOptString = do
  LLVM.setClangOptString (fromMaybe "" mClangOptString)
  Module.setMainModule newModule

initializeForTarget :: App ()
initializeForTarget = do
  Clarify.registerFoundationalTypes
  Unravel.initialize
  Remark.setGlobalRemarkList []
  Global.clearSourceNameMap

initializeForSource :: Source.Source -> App ()
initializeForSource source = do
  UnusedVariable.initialize
  Remark.initialize
  Global.initialize
  Env.setCurrentSource source
  Alias.initializeAliasMap
  Locator.initialize
  Tag.initialize
  Remark.setRemarkList []
  Decl.initialize
