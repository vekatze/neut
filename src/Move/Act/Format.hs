module Move.Act.Format
  ( Handle,
    new,
    format,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Context.App
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Context.Locator qualified as Locator
import Move.Context.Parse (ensureExistence', readTextFile)
import Move.Context.Parse qualified as Parse
import Move.Context.Tag qualified as Tag
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Format qualified as Format
import Move.Scene.Init.Compiler qualified as InitCompiler
import Move.Scene.Init.Target qualified as InitTarget
import Move.Scene.Write qualified as Write
import Path.IO
import Rule.Config.Format

data Handle
  = Handle
  { initCompilerHandle :: InitCompiler.Handle,
    initTargetHandle :: InitTarget.Handle,
    formatHandle :: Format.Handle
  }

new :: Env.Handle -> Gensym.Handle -> Locator.Handle -> Tag.Handle -> App Handle
new envHandle gensymHandle locatorHandle tagHandle = do
  initCompilerHandle <- InitCompiler.new envHandle gensymHandle
  initTargetHandle <- InitTarget.new envHandle gensymHandle locatorHandle tagHandle
  formatHandle <- Format.new envHandle gensymHandle locatorHandle tagHandle
  return $ Handle {..}

format :: Handle -> Config -> EIO ()
format h cfg = do
  InitCompiler.initializeCompiler (initCompilerHandle h) (remarkCfg cfg)
  liftIO $ InitTarget.initializeForTarget (initTargetHandle h)
  path <- resolveFile' $ filePathString cfg
  ensureExistence' path Nothing
  content <- liftIO $ readTextFile path
  content' <- Format.format (formatHandle h) (shouldMinimizeImports cfg) (inputFileType cfg) path content
  if mustUpdateInPlace cfg
    then liftIO $ Write.write path content'
    else liftIO $ Parse.printTextFile content'
