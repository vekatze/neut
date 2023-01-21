module Context.CodataDefinition
  ( Context (..),
  )
where

import Entity.Arity qualified as A
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Prelude hiding (lookup, read)

class Monad m => Context m where
  insert :: DD.DefiniteDescription -> (DD.DefiniteDescription, A.Arity) -> [DD.DefiniteDescription] -> m ()
  lookup :: Hint -> DD.DefiniteDescription -> m ((DD.DefiniteDescription, A.Arity), [DD.DefiniteDescription])
