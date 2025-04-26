module Move.Act.Format (format) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Parse (ensureExistence', readTextFile)
import Move.Context.Parse qualified as Parse
import Move.Scene.Format qualified as Format
import Move.Scene.Initialize qualified as Initialize
import Move.Scene.Module.Reflect qualified as ModuleReflect
import Move.Scene.Write qualified as Write
import Path.IO
import Rule.Config.Format

format :: Config -> App ()
format cfg = do
  h <- ModuleReflect.new
  Initialize.initializeCompiler h (remarkCfg cfg)
  Initialize.initializeForTarget
  path <- resolveFile' $ filePathString cfg
  toApp $ ensureExistence' path Nothing
  content <- liftIO $ readTextFile path
  content' <- Format.format (shouldMinimizeImports cfg) (inputFileType cfg) path content
  if mustUpdateInPlace cfg
    then liftIO $ Write.write path content'
    else liftIO $ Parse.printTextFile content'
