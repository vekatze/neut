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
import Context.Log qualified as Log
import Context.Module qualified as Module
import Context.Path qualified as Path
import Data.Maybe
import Entity.Config.Log qualified as Log
import Entity.Module
import Entity.Source qualified as Source
import Scene.Clarify qualified as Clarify
import Scene.Module.Reflect qualified as Module

initializeCompiler :: Log.Config -> Bool -> Maybe String -> App ()
initializeCompiler cfg shouldCancelAlloc mClangOptString = do
  mainModule <- Module.fromCurrentPath
  initializeCompilerWithModule mainModule cfg shouldCancelAlloc mClangOptString

initializeCompilerWithModule :: Module -> Log.Config -> Bool -> Maybe String -> App ()
initializeCompilerWithModule newModule cfg shouldCancelAlloc mClangOptString = do
  Log.setEndOfEntry $ Log.endOfEntry cfg
  Log.setShouldColorize $ Log.shouldColorize cfg
  Env.setShouldCancelAlloc shouldCancelAlloc
  Env.setTargetPlatform
  LLVM.setClangOptString (fromMaybe "" mClangOptString)
  Path.ensureNotInLibDir
  Module.setMainModule newModule

initializeForTarget :: App ()
initializeForTarget = do
  Global.initialize
  Clarify.registerFoundationalTypes

initializeForSource :: Source.Source -> App ()
initializeForSource source = do
  Env.setCurrentSource source
  Alias.initializeAliasMap
  Locator.initialize
