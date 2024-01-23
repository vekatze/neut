module Act.Zen (zen) where

import Act.Build (Axis (..), buildTarget)
import Context.App
import Context.Env qualified as Env
import Context.Module qualified as Module
import Context.Path qualified as Path
import Control.Monad
import Data.Maybe
import Entity.Config.Zen
import Entity.OutputKind
import Entity.Target
import Path.IO (resolveFile')
import Scene.Fetch qualified as Fetch
import Scene.Initialize qualified as Initialize
import Prelude hiding (log)

zen :: Config -> App ()
zen cfg = do
  setup cfg
  path <- resolveFile' (filePathString cfg)
  buildTarget (fromConfig cfg) $ Concrete $ Zen path

fromConfig :: Config -> Axis
fromConfig cfg =
  Axis
    { _outputKindList = [Object],
      _shouldSkipLink = False,
      _shouldExecute = True,
      _installDir = Nothing,
      _executeArgs = args cfg
    }

setup :: Config -> App ()
setup cfg = do
  Path.ensureNotInLibDir
  Initialize.initializeCompiler (remarkCfg cfg) (mClangOptString cfg)
  Env.setBuildMode $ buildMode cfg
  Module.getMainModule >>= Fetch.fetch
