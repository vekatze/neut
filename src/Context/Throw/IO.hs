module Context.Throw.IO
  ( new,
    Config (..),
  )
where

import qualified Context.Throw as Throw
import qualified Control.Exception.Safe as Safe

data Config = Config
  {
  }

new :: Config -> IO Throw.Context
new _ =
  return
    Throw.Context
      { Throw.throw = Safe.throw,
        Throw.try = Safe.try
      }
