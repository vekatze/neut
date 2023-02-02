module Context.CodataDefinition
  ( Context (..),
  )
where

import qualified Entity.Arity as A
import qualified Entity.DefiniteDescription as DD
import Entity.Hint
import Prelude hiding (lookup, read)

class Monad m => Context m where
  insert :: DD.DefiniteDescription -> (DD.DefiniteDescription, A.Arity) -> [DD.DefiniteDescription] -> m ()
  lookup :: Hint -> DD.DefiniteDescription -> m ((DD.DefiniteDescription, A.Arity), [DD.DefiniteDescription])
