module Scene.Initialize
  ( initializeCompiler,
    initializeCompilerWithModule,
    initializeForTarget,
    initializeForSource,
  )
where

import Context.Alias qualified as Alias
import Context.App
import Context.Env qualified as Env
import Context.Global qualified as Global
import Context.LLVM qualified as LLVM
import Context.Locator qualified as Locator
import Context.Module qualified as Module
import Context.Path qualified as Path
import Context.Remark qualified as Remark
import Context.Tag qualified as Tag
import Context.UnusedVariable qualified as UnusedVariable
import Data.Maybe
import Entity.Config.Remark qualified as Remark
import Entity.Module
import Entity.Source qualified as Source
import Scene.Clarify qualified as Clarify
import Scene.Module.Reflect qualified as Module

initializeCompiler :: Remark.Config -> Maybe String -> App ()
initializeCompiler cfg mClangOptString = do
  mainModule <- Module.fromCurrentPath
  initializeCompilerWithModule mainModule cfg mClangOptString

initializeCompilerWithModule :: Module -> Remark.Config -> Maybe String -> App ()
initializeCompilerWithModule newModule cfg mClangOptString = do
  Remark.setEndOfEntry $ Remark.endOfEntry cfg
  Remark.setShouldColorize $ Remark.shouldColorize cfg
  Env.setTargetPlatform
  LLVM.setClangOptString (fromMaybe "" mClangOptString)
  Path.ensureNotInLibDir
  Module.setMainModule newModule

initializeForTarget :: App ()
initializeForTarget = do
  Clarify.registerFoundationalTypes

initializeForSource :: Source.Source -> App ()
initializeForSource source = do
  UnusedVariable.initialize
  Remark.initialize
  Global.initialize
  Env.setCurrentSource source
  Alias.initializeAliasMap
  Locator.initialize
  Tag.initialize
