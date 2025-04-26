module Move.Scene.Initialize
  ( initializeCompiler,
    initializeCompilerWithModule,
    initializeCompilerWithPath,
  )
where

import Control.Monad.IO.Class
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Scene.Init.Logger qualified as InitLogger
import Move.Scene.Module.Reflect qualified as ModuleReflect
import Path
import Rule.Config.Remark qualified as Remark
import Rule.Module

initializeCompiler :: ModuleReflect.Handle -> Remark.Config -> App ()
initializeCompiler h cfg = do
  h' <- InitLogger.new
  liftIO $ InitLogger.initializeLogger h' cfg
  mainModule <- toApp $ ModuleReflect.fromCurrentPath h
  initializeCompilerWithModule mainModule

initializeCompilerWithPath :: ModuleReflect.Handle -> Path Abs File -> Remark.Config -> App ()
initializeCompilerWithPath h path cfg = do
  h' <- InitLogger.new
  liftIO $ InitLogger.initializeLogger h' cfg
  mainModule <- toApp $ ModuleReflect.fromFilePath h path
  initializeCompilerWithModule mainModule

initializeCompilerWithModule :: Module -> App ()
initializeCompilerWithModule newModule = do
  Env.setMainModule (MainModule newModule)
