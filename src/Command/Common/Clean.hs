module Command.Common.Clean
  ( Handle,
    new,
    clean,
  )
where

import App.App (App)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.HashSet qualified as HashSet
import Data.Text qualified as T
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.Module qualified as Module
import Kernel.Common.Handle.Global.Platform qualified as Platform
import Kernel.Common.Module qualified as M
import Kernel.Common.Platform qualified as P
import Kernel.Unravel.Unravel qualified as Unravel
import Language.Common.ModuleDigest qualified as MD
import Language.Common.ModuleID qualified as MID
import Path (toFilePath)
import Path.IO
import System.Directory qualified as D
import System.FilePath qualified as FP
import Prelude hiding (log)

data Handle = Handle
  { envHandle :: Env.Handle,
    unravelHandle :: Unravel.Handle
  }

new :: Global.Handle -> IO Handle
new globalHandle = do
  let envHandle = Global.envHandle globalHandle
  unravelHandle <- Unravel.new globalHandle
  return $ Handle {..}

clean :: Handle -> App ()
clean h = do
  let mainModule = M.extractModule $ Env.getMainModule (envHandle h)
  moduleList <- Unravel.unravelModule (unravelHandle h) mainModule
  forM_ moduleList $ \someModule -> do
    forM_ [minBound .. maxBound :: P.PlatformName] $ \name -> do
      baseBuildDir <- Platform.getBaseBuildDir name someModule
      b <- doesDirExist baseBuildDir
      when b $ do
        removeDirRecur baseBuildDir
  removeUnusedDependencies mainModule moduleList

removeUnusedDependencies :: M.Module -> [M.Module] -> App ()
removeUnusedDependencies mainModule usedModuleList = do
  case Module.getOwnedDependencyDir mainModule of
    Nothing ->
      return ()
    Just dependencyDir -> do
      dependencyDirExists <- doesDirExist dependencyDir
      when dependencyDirExists $ do
        let usedModuleIDSet = HashSet.fromList $ map M.moduleID usedModuleList
        entryList <- liftIO $ D.listDirectory $ toFilePath dependencyDir
        forM_ entryList $ \entry -> do
          unless (HashSet.member (MID.Library $ MD.ModuleDigest $ T.pack entry) usedModuleIDSet) $ do
            liftIO $ D.removePathForcibly $ toFilePath dependencyDir FP.</> entry
