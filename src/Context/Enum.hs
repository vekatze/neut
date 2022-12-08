module Context.Enum
  ( Context (..),
  )
where

import qualified Entity.DefiniteDescription as DD

class Monad m => Context m where
  insert :: DD.DefiniteDescription -> m ()
  isMember :: DD.DefiniteDescription -> m Bool
