module Move.Act.Get
  ( Handle,
    new,
    get,
  )
where

import Control.Monad
import Move.Context.EIO (EIO)
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

get :: Handle -> Config -> EIO ()
get h cfg = do
  InitCompiler.initializeCompiler (initCompilerHandle h) (remarkCfg cfg)
  mainModule <- Env.getMainModule (envHandle h)
  Path.ensureNotInDependencyDir mainModule
  Clean.clean (cleanHandle h)
  Fetch.insertDependency (fetchHandle h) (moduleAliasText cfg) (moduleURL cfg)
  InitCompiler.initializeCompilerWithPath (initCompilerHandle h) (moduleLocation (extractModule mainModule)) (remarkCfg cfg)
  void $ Check.checkAll (checkHandle h)
