module Move.Console.EnsureExecutables (ensureExecutables) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Context.App
import Move.Context.Clang qualified as Clang
import Move.Context.EIO (toApp)
import Move.Context.External (ensureExecutable)

ensureExecutables :: App ()
ensureExecutables = do
  clang <- liftIO Clang.getClang
  mapM_
    (toApp . ensureExecutable)
    [ clang,
      "curl",
      "tar",
      "zstd"
    ]
