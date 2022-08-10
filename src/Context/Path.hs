module Context.Path where

import qualified Context.Throw as Throw
import Path

class Throw.Context m => Context m where
  getLibraryDirPath :: m (Path Abs Dir)
  ensureNotInLibDir :: m ()

-- newtype Config = Config
--   { throwCtx :: Throw.Context
--   }
