module Move.Act.Format
  ( Handle,
    new,
    format,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Context.EIO (EIO)
import Move.Context.Parse (ensureExistence', readTextFile)
import Move.Context.Parse qualified as Parse
import Move.Scene.Format qualified as Format
import Move.Scene.Init.Base qualified as Base
import Move.Scene.Write qualified as Write
import Path.IO
import Rule.Config.Format

newtype Handle
  = Handle
  { baseHandle :: Base.Handle
  }

new :: Base.Handle -> Handle
new baseHandle = do
  Handle {..}

format :: Handle -> Config -> EIO ()
format h cfg = do
  path <- resolveFile' $ filePathString cfg
  ensureExistence' path Nothing
  content <- liftIO $ readTextFile path
  let formatHandle = Format.new (baseHandle h)
  content' <- Format.format formatHandle (shouldMinimizeImports cfg) (inputFileType cfg) path content
  if mustUpdateInPlace cfg
    then liftIO $ Write.write path content'
    else liftIO $ Parse.printTextFile content'
