module Scene.Install (install) where

import Context.App
import Context.Module qualified as Module
import Context.Path qualified as Path
import Control.Monad
import Data.Text qualified as T
import Entity.Target qualified as Target
import Path
import Path.IO
import Prelude hiding (log)

install :: Target.Target -> Path Abs Dir -> App ()
install target dir = do
  execPath <- Module.getMainModule >>= Path.getExecutableOutputPath target
  execName <- parseRelFile $ T.unpack $ Target.extract target
  let destPath = dir </> execName
  copyFile execPath destPath
