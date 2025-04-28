module Move.Act.Get (get) where

import Control.Monad
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Context.Path qualified as Path
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Check qualified as Check
import Move.Scene.Clean qualified as Clean
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Init.Compiler qualified as InitCompiler
import Rule.Config.Get
import Rule.Module
import Prelude hiding (log)

get :: Config -> App ()
get cfg = do
  gensymHandle <- Gensym.new
  hc <- InitCompiler.new
  toApp $ InitCompiler.initializeCompiler hc (remarkCfg cfg)
  envHandle <- Env.new
  mainModule <- toApp $ Env.getMainModule envHandle
  toApp $ Path.ensureNotInDependencyDir mainModule
  cleanHandle <- Clean.new
  toApp $ Clean.clean cleanHandle
  h <- Fetch.new
  toApp $ Fetch.insertDependency h (moduleAliasText cfg) (moduleURL cfg)
  toApp $ InitCompiler.initializeCompilerWithPath hc (moduleLocation (extractModule mainModule)) (remarkCfg cfg)
  hck <- Check.new gensymHandle
  void $ Check.checkAll hck
