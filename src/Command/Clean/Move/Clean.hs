module Command.Clean.Move.Clean
  ( Handle,
    new,
    clean,
  )
where

import Command.Common.Move.Clean qualified as Clean
import Error.Rule.EIO (EIO)
import Kernel.Move.Scene.Init.Base qualified as Base
import Prelude hiding (log)

newtype Handle = Handle
  { cleanHandle :: Clean.Handle
  }

new :: Base.Handle -> IO Handle
new baseHandle = do
  cleanHandle <- Clean.new baseHandle
  return $ Handle {..}

clean :: Handle -> EIO ()
clean h = do
  Clean.clean (cleanHandle h)
