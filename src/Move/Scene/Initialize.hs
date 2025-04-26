module Move.Scene.Initialize
  ( initializeCompiler,
    initializeCompilerWithModule,
    initializeCompilerWithPath,
    initializeLogger,
  )
where

import Control.Monad.IO.Class
import Move.Console.Report qualified as Report
import Move.Context.App
import Move.Context.Color qualified as Color
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Scene.Module.Reflect qualified as ModuleReflect
import Path
import Rule.Config.Remark qualified as Remark
import Rule.Module

initializeLogger :: Remark.Config -> App ()
initializeLogger cfg = do
  h <- Color.new
  liftIO $ Color.setShouldColorizeStdout h $ Remark.shouldColorize cfg
  liftIO $ Color.setShouldColorizeStderr h $ Remark.shouldColorize cfg
  hr <- Report.new
  liftIO $ Report.setEndOfEntry hr $ Remark.endOfEntry cfg
  Env.setSilentMode $ Remark.enableSilentMode cfg
  hd <- Debug.new
  liftIO $ Debug.setDebugMode hd $ Remark.enableDebugMode cfg

initializeCompiler :: ModuleReflect.Handle -> Remark.Config -> App ()
initializeCompiler h cfg = do
  initializeLogger cfg
  mainModule <- toApp $ ModuleReflect.fromCurrentPath h
  initializeCompilerWithModule mainModule

initializeCompilerWithPath :: ModuleReflect.Handle -> Path Abs File -> Remark.Config -> App ()
initializeCompilerWithPath h path cfg = do
  initializeLogger cfg
  mainModule <- toApp $ ModuleReflect.fromFilePath h path
  initializeCompilerWithModule mainModule

initializeCompilerWithModule :: Module -> App ()
initializeCompilerWithModule newModule = do
  Env.setMainModule (MainModule newModule)
