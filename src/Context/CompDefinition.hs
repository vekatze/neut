module Context.CompDefinition
  ( Context (..),
    DefKey,
    DefValue,
    DefMap,
  )
where

import Data.HashMap.Strict qualified as Map
import Entity.Comp
import Entity.DefiniteDescription qualified as DD
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
