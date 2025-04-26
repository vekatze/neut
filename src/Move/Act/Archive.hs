module Move.Act.Archive (archive) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Context.Path qualified as Path
import Move.Scene.Archive qualified as Archive
import Move.Scene.Collect qualified as Collect
import Move.Scene.Ens.Reflect qualified as EnsReflect
import Move.Scene.Init.Compiler qualified as InitCompiler
import Move.Scene.Module.MakeArchiveEns
import Move.Scene.PackageVersion.ChooseNewVersion qualified as PV
import Move.Scene.PackageVersion.Reflect qualified as PV
import Rule.Config.Archive

archive :: Config -> App ()
archive cfg = do
  h <- InitCompiler.new
  toApp $ InitCompiler.initializeCompiler h (remarkCfg cfg)
  envHandle <- Env.new
  mainModule <- liftIO $ Env.getMainModule envHandle
  toApp $ Path.ensureNotInDependencyDir mainModule
  hp <- PV.new
  packageVersion <- toApp $ maybe (PV.chooseNewVersion hp mainModule) (PV.reflect mainModule) (getArchiveName cfg)
  h' <- EnsReflect.new
  archiveEns <- toApp $ makeArchiveEns h' packageVersion mainModule
  let (moduleRootDir, contents) = Collect.collectModuleFiles mainModule
  h'' <- Archive.new
  toApp $ Archive.archive h'' packageVersion archiveEns moduleRootDir contents
