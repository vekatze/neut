module Move.Scene.Init.Compiler
  ( Handle,
    new,
    initializeCompiler,
    initializeCompilerWithModule,
    initializeCompilerWithPath,
  )
where

import Control.Monad.IO.Class
import Move.Context.App
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Scene.Init.Logger qualified as InitLogger
import Move.Scene.Module.Reflect qualified as ModuleReflect
import Path
import Rule.Config.Remark qualified as Remark
import Rule.Module

data Handle
  = Handle
  { initLoggerHandle :: InitLogger.Handle,
    moduleReflectHandle :: ModuleReflect.Handle,
    envHandle :: Env.Handle
  }

new :: App Handle
new = do
  initLoggerHandle <- InitLogger.new
  moduleReflectHandle <- ModuleReflect.new
  envHandle <- Env.new
  return $ Handle {..}

initializeCompiler :: Handle -> Remark.Config -> EIO ()
initializeCompiler h cfg = do
  liftIO $ InitLogger.initializeLogger (initLoggerHandle h) cfg
  mainModule <- ModuleReflect.fromCurrentPath (moduleReflectHandle h)
  liftIO $ initializeCompilerWithModule h mainModule

initializeCompilerWithPath :: Handle -> Path Abs File -> Remark.Config -> EIO ()
initializeCompilerWithPath h path cfg = do
  liftIO $ InitLogger.initializeLogger (initLoggerHandle h) cfg
  mainModule <- ModuleReflect.fromFilePath (moduleReflectHandle h) path
  liftIO $ initializeCompilerWithModule h mainModule

initializeCompilerWithModule :: Handle -> Module -> IO ()
initializeCompilerWithModule h newModule = do
  Env.setMainModule (envHandle h) (MainModule newModule)
