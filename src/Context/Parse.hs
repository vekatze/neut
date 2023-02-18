module Context.Parse
  ( readSourceFile,
    ensureExistence,
  )
where

import Context.App
import Context.Throw qualified as Throw
import Control.Monad
import Control.Monad.IO.Class
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Path
import Path.IO

readSourceFile :: Path Abs File -> App T.Text
readSourceFile =
  liftIO . TIO.readFile . toFilePath

ensureExistence :: Path Abs File -> App ()
ensureExistence path = do
  fileExists <- doesFileExist path
  unless fileExists $ do
    Throw.raiseError' $ T.pack $ "no such file exists: " <> toFilePath path
