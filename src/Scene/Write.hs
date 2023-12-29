module Scene.Write (write) where

import Context.App
import Context.Parse
import Data.Text qualified as T
import Path.IO
import Prelude hiding (log)

write :: FilePath -> T.Text -> App ()
write path content = do
  path' <- resolveFile' path
  writeSourceFile path' content
