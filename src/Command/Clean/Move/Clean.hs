module Command.Clean.Move.Clean
  ( Handle,
    new,
    clean,
  )
where

import Error.Rule.EIO (EIO)
import Command.Common.Move.Clean qualified as Clean
import Kernel.Common.Move.CreateGlobalHandle qualified as Global
import Prelude hiding (log)

newtype Handle = Handle
  { cleanHandle :: Clean.Handle
  }

new :: Global.Handle -> IO Handle
new globalHandle = do
  cleanHandle <- Clean.new globalHandle
  return $ Handle {..}

clean :: Handle -> EIO ()
clean h = do
  Clean.clean (cleanHandle h)
