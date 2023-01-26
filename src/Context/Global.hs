module Context.Global where

import Entity.Arity
import Entity.Binder
import Entity.DefiniteDescription qualified as DD
import Entity.Discriminant qualified as D
import Entity.GlobalName
import Entity.Hint

class Monad m => Context m where
  registerTopLevelFunc :: Hint -> DD.DefiniteDescription -> Arity -> m ()
  registerData ::
    Hint ->
    DD.DefiniteDescription ->
    [BinderF a] ->
    [(DD.DefiniteDescription, [BinderF a], D.Discriminant)] ->
    m ()
  registerResource :: Hint -> DD.DefiniteDescription -> m ()
  lookup :: DD.DefiniteDescription -> m (Maybe GlobalName)
  initialize :: m ()
