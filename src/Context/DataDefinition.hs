module Context.DataDefinition
  ( Context (..),
  )
where

import Entity.Binder
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Discriminant as D
import Entity.Term
import Prelude hiding (lookup, read)

class Monad m => Context m where
  insert :: DD.DefiniteDescription -> D.Discriminant -> [BinderF Term] -> [BinderF Term] -> m ()
  lookup :: DD.DefiniteDescription -> m (Maybe [(D.Discriminant, [BinderF Term], [BinderF Term])])