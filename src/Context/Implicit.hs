module Context.Implicit
  ( Context (..),
  )
where

import qualified Entity.DefiniteDescription as DD
import qualified Entity.ImpArgNum as I

class Monad m => Context m where
  insert :: DD.DefiniteDescription -> I.ImpArgNum -> m ()
  lookup :: DD.DefiniteDescription -> m (Maybe I.ImpArgNum)
