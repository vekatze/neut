module Move.Console.EnsureExecutables (ensureExecutables) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Context.EIO (EIO)
import Move.Context.External (ensureExecutable)
import Move.Context.Platform qualified as Platform

ensureExecutables :: EIO ()
ensureExecutables = do
  clang <- liftIO Platform.getClang
  mapM_
    ensureExecutable
    [ clang,
      "curl",
      "tar",
      "zstd"
    ]
