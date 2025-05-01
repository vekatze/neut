module Move.Scene.Init.Compiler
  ( Handle,
    new,
    initializeCompiler,
  )
where

import Control.Monad.IO.Class
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Scene.Init.Logger qualified as InitLogger
import Move.Scene.Module.Reflect qualified as ModuleReflect
import Rule.Config.Remark qualified as Remark

-- import Rule.Module

data Handle
  = Handle
  { initLoggerHandle :: InitLogger.Handle,
    moduleReflectHandle :: ModuleReflect.Handle,
    envHandle :: Env.Handle
  }

new :: InitLogger.Handle -> ModuleReflect.Handle -> Env.Handle -> Handle
new initLoggerHandle moduleReflectHandle envHandle = do
  Handle {..}

initializeCompiler :: Handle -> Remark.Config -> EIO ()
initializeCompiler h cfg = do
  liftIO $ InitLogger.initializeLogger (initLoggerHandle h) cfg

-- mainModule <- ModuleReflect.fromCurrentPath (moduleReflectHandle h)

-- liftIO $ initializeCompilerWithModule h mainModule

-- mainModule <- ModuleReflect.fromFilePath (moduleReflectHandle h) path

-- liftIO $ initializeCompilerWithModule h mainModule

-- initializeCompilerWithModule :: Handle -> Module -> IO ()
-- initializeCompilerWithModule h newModule = do
--   Env.setMainModule (envHandle h) (MainModule newModule)
