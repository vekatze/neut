module Context.Global where

import Entity.Arity
import qualified Entity.DefiniteDescription as DD
import Entity.EnumInfo
import qualified Entity.EnumTypeName as ET
import Entity.GlobalName
import Entity.Hint

class Monad m => Context m where
  registerTopLevelFunc :: Hint -> DD.DefiniteDescription -> Arity -> m ()
  registerEnum :: Hint -> ET.EnumTypeName -> [EnumValue] -> m ()
  registerResource :: Hint -> DD.DefiniteDescription -> m ()
  registerData :: Hint -> DD.DefiniteDescription -> Arity -> [DD.DefiniteDescription] -> m ()
  lookup :: DD.DefiniteDescription -> m (Maybe GlobalName)
  initialize :: m ()
