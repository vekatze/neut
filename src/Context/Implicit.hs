module Context.Implicit
  ( Context (..),
  )
where

import Entity.DefiniteDescription qualified as DD
import Entity.ImpArgNum qualified as I

class Monad m => Context m where
  insert :: DD.DefiniteDescription -> I.ImpArgNum -> m ()
  lookup :: DD.DefiniteDescription -> m (Maybe I.ImpArgNum)
