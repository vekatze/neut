module Move.Console.EnsureExecutables (ensureExecutables) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Context.Clang qualified as Clang
import Move.Context.EIO (EIO)
import Move.Context.External (ensureExecutable)

ensureExecutables :: EIO ()
ensureExecutables = do
  clang <- liftIO Clang.getClang
  mapM_
    ensureExecutable
    [ clang,
      "curl",
      "tar",
      "zstd"
    ]
