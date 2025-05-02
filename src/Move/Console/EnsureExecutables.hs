module Move.Console.EnsureExecutables (ensureExecutables) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Context.External (ensureExecutable)

ensureExecutables :: EIO ()
ensureExecutables = do
  clang <- liftIO Env.getClang
  mapM_
    ensureExecutable
    [ clang,
      "curl",
      "tar",
      "zstd"
    ]
