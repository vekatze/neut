module Context.Type
  ( Context (..),
  )
where

import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.WeakTerm

class Monad m => Context m where
  lookup :: Hint -> DD.DefiniteDescription -> m WeakTerm
  insert :: DD.DefiniteDescription -> WeakTerm -> m ()
