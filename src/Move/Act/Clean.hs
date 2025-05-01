module Move.Act.Clean
  ( Handle,
    new,
    clean,
  )
where

import Move.Context.EIO (EIO)
import Move.Scene.Clean qualified as Clean
import Move.Scene.Init.Base qualified as Base
import Prelude hiding (log)

newtype Handle
  = Handle
  { cleanHandle :: Clean.Handle
  }

new :: Base.Handle -> IO Handle
new baseHandle = do
  cleanHandle <- Clean.new baseHandle
  return $ Handle {..}

clean :: Handle -> EIO ()
clean h = do
  Clean.clean (cleanHandle h)
