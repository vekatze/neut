module Context.Gensym.Main
  ( new,
  )
where

import Context.Gensym
import Data.IORef.Unboxed
import Prelude hiding (log)

new :: IO Axis
new = do
  counter <- newCounter 0
  return
    Axis
      { newCount = atomicAddCounter counter 1,
        readCount = readIORefU counter,
        writeCount = writeIORefU counter
      }
