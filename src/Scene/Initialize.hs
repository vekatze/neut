module Scene.Initialize
  ( Context (),
    initializeCompiler,
    initializeCompilerWithModule,
    initializeForTarget,
    initializeForSource,
  )
where

import Context.Alias qualified as Alias
import Context.Env qualified as Env
import Context.Global qualified as Global
import Context.Locator qualified as Locator
import Context.Log qualified as Log
import Context.Module qualified as Module
import Context.Path qualified as Path
import Context.Throw qualified as Throw
import Entity.Module
import Entity.Source qualified as Source
import Scene.Clarify qualified as Clarify
import Scene.Module.Reflect qualified as Module
import Scene.Parse.Core qualified as Parse

class
  ( Throw.Context m,
    Log.Context m,
    Alias.Context m,
    Path.Context m,
    Global.Context m,
    Module.Context m,
    Clarify.Context m,
    Parse.Context m,
    Env.Context m
  ) =>
  Context m

initializeCompiler :: Context m => Log.Config -> Bool -> m ()
initializeCompiler cfg shouldCancelAlloc = do
  mainModule <- Module.fromCurrentPath
  initializeCompilerWithModule mainModule cfg shouldCancelAlloc

initializeCompilerWithModule :: Context m => Module -> Log.Config -> Bool -> m ()
initializeCompilerWithModule newModule cfg shouldCancelAlloc = do
  Env.setEndOfEntry $ Log.endOfEntry cfg
  Env.setShouldColorize $ Log.shouldColorize cfg
  Env.setShouldCancelAlloc shouldCancelAlloc
  Env.setTargetPlatform
  Path.ensureNotInLibDir
  Env.setMainModule newModule

initializeForTarget :: Context m => m ()
initializeForTarget = do
  Global.initialize
  Clarify.registerFoundationalTypes

initializeForSource :: Context m => Source.Source -> m ()
initializeForSource source = do
  Env.setCurrentSource source
  Alias.initializeAliasMap
  Locator.initialize
