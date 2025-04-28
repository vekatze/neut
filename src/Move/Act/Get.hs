module Move.Act.Get
  ( Handle,
    new,
    get,
  )
where

import Control.Monad
import Move.Console.Report qualified as Report
import Move.Context.Antecedent qualified as Antecedent
import Move.Context.App
import Move.Context.Color qualified as Color
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Context.KeyArg qualified as KeyArg
import Move.Context.Locator qualified as Locator
import Move.Context.OptimizableData qualified as OptimizableData
import Move.Context.Path qualified as Path
import Move.Context.Tag qualified as Tag
import Move.Context.Unused qualified as Unused
import Move.Language.Utility.Gensym qualified as Gensym
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
  Env.Handle ->
  Gensym.Handle ->
  Color.Handle ->
  Report.Handle ->
  Debug.Handle ->
  Locator.Handle ->
  OptimizableData.Handle ->
  KeyArg.Handle ->
  Unused.Handle ->
  Tag.Handle ->
  Antecedent.Handle ->
  App Handle
new envHandle gensymHandle colorHandle reportHandle debugHandle locatorHandle optDataHandle keyArgHandle unusedHandle tagHandle antecedentHandle = do
  initCompilerHandle <- InitCompiler.new envHandle gensymHandle colorHandle reportHandle debugHandle
  fetchHandle <- Fetch.new envHandle gensymHandle reportHandle debugHandle
  cleanHandle <- Clean.new envHandle gensymHandle debugHandle locatorHandle optDataHandle keyArgHandle unusedHandle tagHandle antecedentHandle
  checkHandle <- Check.new envHandle gensymHandle colorHandle debugHandle locatorHandle optDataHandle keyArgHandle unusedHandle tagHandle antecedentHandle
  return $ Handle {..}

get :: Handle -> Config -> App ()
get h cfg = do
  toApp $ InitCompiler.initializeCompiler (initCompilerHandle h) (remarkCfg cfg)
  mainModule <- toApp $ Env.getMainModule (envHandle h)
  toApp $ Path.ensureNotInDependencyDir mainModule
  toApp $ Clean.clean (cleanHandle h)
  toApp $ Fetch.insertDependency (fetchHandle h) (moduleAliasText cfg) (moduleURL cfg)
  toApp $ InitCompiler.initializeCompilerWithPath (initCompilerHandle h) (moduleLocation (extractModule mainModule)) (remarkCfg cfg)
  void $ Check.checkAll (checkHandle h)
