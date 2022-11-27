module Context.Global where

import Entity.Arity
import qualified Entity.DefiniteDescription as DD
import qualified Entity.Discriminant as D
import Entity.EnumInfo
import qualified Entity.EnumTypeName as ET
import Entity.GlobalName
import Entity.Hint

class Monad m => Context m where
  registerTopLevelFunc :: Hint -> DD.DefiniteDescription -> Arity -> m ()
  registerEnum :: Hint -> ET.EnumTypeName -> [EnumValue] -> m ()
  registerData :: Hint -> DD.DefiniteDescription -> Arity -> [DD.DefiniteDescription] -> m ()
  registerDataIntro :: Hint -> DD.DefiniteDescription -> Arity -> Arity -> D.Discriminant -> m ()
  lookup :: DD.DefiniteDescription -> m (Maybe GlobalName)
  initialize :: m ()
