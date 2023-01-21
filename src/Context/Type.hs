module Context.Type
  ( Context (..),
  )
where

import Context.Throw qualified as Throw
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.WeakTerm

class Throw.Context m => Context m where
  lookup :: Hint -> DD.DefiniteDescription -> m WeakTerm
  insert :: DD.DefiniteDescription -> WeakTerm -> m ()
