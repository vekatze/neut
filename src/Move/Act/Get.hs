module Move.Act.Get
  ( Handle,
    new,
    get,
  )
where

import Control.Monad
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Context.Path qualified as Path
import Move.Scene.Check qualified as Check
import Move.Scene.Clean qualified as Clean
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Init.Compiler qualified as InitCompiler
import Rule.Config.Get
import Rule.Module
import Prelude hiding (log)

data Handle
  = Handle
  { initCompilerHandle :: InitCompiler.Handle,
    fetchHandle :: Fetch.Handle,
    envHandle :: Env.Handle,
    cleanHandle :: Clean.Handle,
    checkHandle :: Check.Handle
  }

new ::
  InitCompiler.Handle ->
  Fetch.Handle ->
  Env.Handle ->
  Clean.Handle ->
  Check.Handle ->
  Handle
new initCompilerHandle fetchHandle envHandle cleanHandle checkHandle = do
  Handle {..}

get :: Handle -> Config -> App ()
get h cfg = do
  toApp $ InitCompiler.initializeCompiler (initCompilerHandle h) (remarkCfg cfg)
  mainModule <- toApp $ Env.getMainModule (envHandle h)
  toApp $ Path.ensureNotInDependencyDir mainModule
  toApp $ Clean.clean (cleanHandle h)
  toApp $ Fetch.insertDependency (fetchHandle h) (moduleAliasText cfg) (moduleURL cfg)
  toApp $ InitCompiler.initializeCompilerWithPath (initCompilerHandle h) (moduleLocation (extractModule mainModule)) (remarkCfg cfg)
  void $ toApp $ Check.checkAll (checkHandle h)
