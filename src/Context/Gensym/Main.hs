module Context.Gensym.Main
  ( new,
  )
where

import qualified Context.Gensym as Gensym
import Data.IORef.Unboxed
import Prelude hiding (log)

new :: Gensym.Config -> IO Gensym.Axis
new _ = do
  counter <- newCounter 0
  return
    Gensym.Axis
      { Gensym.newCount = atomicAddCounter counter 1,
        Gensym.readCount = readIORefU counter,
        Gensym.writeCount = writeIORefU counter
      }
