module Context.CompDefinition
  ( Context (..),
    Config (..),
    DefKey,
    DefValue,
    DefMap,
  )
where

import qualified Data.HashMap.Strict as Map
import Entity.Comp
import qualified Entity.DefiniteDescription as DD
import Entity.Ident
import Entity.Opacity
import Prelude hiding (lookup, read)

type DefKey = DD.DefiniteDescription

type DefValue = (Opacity, [Ident], Comp)

type DefMap = Map.HashMap DD.DefiniteDescription (Opacity, [Ident], Comp)

data Context = Context
  { insert :: DefKey -> DefValue -> IO (),
    union :: DefMap -> IO (),
    lookup :: DefKey -> IO (Maybe DefValue)
  }

data Config = Config
  {
  }

-- compDefEnvRef :: IORef (Map.HashMap DD.DefiniteDescription (Opacity, [Ident], Comp))
