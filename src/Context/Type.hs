module Context.Type
  ( Context (..),
    Config (..),
  )
where

import qualified Context.Throw as Throw
import qualified Entity.DefiniteDescription as DD
import Entity.Hint
import Entity.WeakTerm

data Context = Context
  { lookup :: Hint -> DD.DefiniteDescription -> IO WeakTerm,
    insert :: DD.DefiniteDescription -> WeakTerm -> IO ()
  }

newtype Config = Config
  { throwCtx :: Throw.Context
  }
