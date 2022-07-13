module Context.Path where

import qualified Context.Throw as Throw
import Path

data Context = Context
  { getLibraryDirPath :: IO (Path Abs Dir),
    ensureNotInLibDir :: IO ()
  }

newtype Config = Config
  { throwCtx :: Throw.Context
  }
