module Context.CompDefinition
  ( Context (..),
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

class Monad m => Context m where
  insert :: DefKey -> DefValue -> m ()
  union :: DefMap -> m ()
  lookup :: DefKey -> m (Maybe DefValue)

-- data Config = Config
--   {
--   }

-- compDefEnvRef :: IORef (Map.HashMap DD.DefiniteDescription (Opacity, [Ident], Comp))
