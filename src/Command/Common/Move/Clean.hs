module Command.Common.Move.Clean
  ( Handle,
    new,
    clean,
  )
where

import Control.Monad
import Kernel.Common.Move.CreateGlobalHandle qualified as Global
import Kernel.Common.Move.Handle.Global.Env qualified as Env
import Kernel.Common.Move.Handle.Global.Platform qualified as Platform
import Kernel.Common.Rule.Handle.Global.Env qualified as Env
import Kernel.Common.Rule.Handle.Global.Path qualified as Path
import Kernel.Common.Rule.Handle.Global.Platform qualified as Platform
import Kernel.Common.Rule.Module (extractModule)
import Kernel.Unravel.Move.Unravel qualified as Unravel
import Library.Error.Rule.EIO (EIO)
import Path.IO
import Prelude hiding (log)

data Handle = Handle
  { envHandle :: Env.Handle,
    pathHandle :: Path.Handle,
    platformHandle :: Platform.Handle,
    unravelHandle :: Unravel.Handle
  }

new :: Global.Handle -> IO Handle
new globalHandle = do
  let envHandle = Global.envHandle globalHandle
  let pathHandle = Global.pathHandle globalHandle
  let platformHandle = Global.platformHandle globalHandle
  unravelHandle <- Unravel.new globalHandle
  return $ Handle {..}

clean :: Handle -> EIO ()
clean h = do
  let mainModule = Env.getMainModule (envHandle h)
  moduleList <- Unravel.unravelModule (unravelHandle h) (extractModule mainModule)
  forM_ moduleList $ \someModule -> do
    baseBuildDir <- Platform.getBaseBuildDir (platformHandle h) someModule
    b <- doesDirExist baseBuildDir
    when b $ do
      removeDirRecur baseBuildDir
    ensureDir baseBuildDir
