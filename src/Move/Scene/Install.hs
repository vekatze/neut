module Move.Scene.Install (install) where

import Control.Monad
import Data.Text qualified as T
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Context.Path qualified as Path
import Path
import Path.IO
import Rule.Module (extractModule)
import Rule.Target qualified as Target
import Prelude hiding (log)

install :: Target.MainTarget -> Path Abs Dir -> App ()
install targetOrZen dir = do
  mainModule <- Env.getMainModule
  h <- Path.new
  execPath <- toApp $ Path.getExecutableOutputPath h targetOrZen (extractModule mainModule)
  case targetOrZen of
    Target.Named targetName _ -> do
      execName <- parseRelFile $ T.unpack targetName
      let destPath = dir </> execName
      copyFile execPath destPath
    Target.Zen {} ->
      return ()
