module Main.Move.Act.Clean
  ( Handle,
    new,
    clean,
  )
where

import Error.Rule.EIO (EIO)
import Main.Move.Scene.Clean qualified as Clean
import Main.Move.Scene.Init.Base qualified as Base
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
