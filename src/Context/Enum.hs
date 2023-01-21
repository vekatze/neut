module Context.Enum
  ( Context (..),
  )
where

import Entity.DefiniteDescription qualified as DD

class Monad m => Context m where
  insert :: DD.DefiniteDescription -> m ()
  isMember :: DD.DefiniteDescription -> m Bool
