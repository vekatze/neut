module Context.Implicit
  ( Context (..),
    Config (..),
  )
where

import qualified Entity.DefiniteDescription as DD
import qualified Entity.ImpArgNum as I

data Context = Context
  { insert :: DD.DefiniteDescription -> I.ImpArgNum -> IO (),
    lookup :: DD.DefiniteDescription -> IO (Maybe I.ImpArgNum)
  }

data Config = Config
  {
  }
