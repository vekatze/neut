module Move.Act.Get (get) where

import Control.Monad
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Env (getMainModule)
import Move.Context.Path qualified as Path
import Move.Scene.Check qualified as Check
import Move.Scene.Clean qualified as Clean
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Initialize qualified as Initialize
import Move.Scene.Module.Reflect qualified as ModuleReflect
import Rule.Config.Get
import Rule.Module
import Prelude hiding (log)

get :: Config -> App ()
get cfg = do
  hm <- ModuleReflect.new
  Initialize.initializeCompiler hm (remarkCfg cfg)
  mainModule <- getMainModule
  toApp $ Path.ensureNotInDependencyDir mainModule
  hc <- Clean.new
  toApp $ Clean.clean hc
  h <- Fetch.new
  toApp $ Fetch.insertDependency h (moduleAliasText cfg) (moduleURL cfg)
  Initialize.initializeCompilerWithPath hm (moduleLocation (extractModule mainModule)) (remarkCfg cfg)
  void Check.checkAll
