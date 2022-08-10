module Context.Type
  ( Context (..),
  )
where

import qualified Context.Throw as Throw
import qualified Entity.DefiniteDescription as DD
import Entity.Hint
import Entity.WeakTerm

class Throw.Context m => Context m where
  lookup :: Hint -> DD.DefiniteDescription -> m WeakTerm
  insert :: DD.DefiniteDescription -> WeakTerm -> m ()

-- newtype Config = Config
--   { throwCtx :: Throw.Context
--   }
