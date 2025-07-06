module Command.Clean.Clean
  ( Handle,
    new,
    clean,
  )
where

import Command.Common.Clean qualified as Clean
import Error.EIO (EIO)
import Kernel.Common.CreateGlobalHandle qualified as Global
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
