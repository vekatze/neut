module Move.Scene.Write (write) where

import Data.Text qualified as T
import Move.Context.Parse
import Path
import Prelude hiding (log)

write :: Path Abs File -> T.Text -> IO ()
write path content = do
  writeTextFile path content
