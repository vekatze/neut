module Context.Global where

import qualified Context.Throw as Throw
import Entity.Arity
import qualified Entity.DefiniteDescription as DD
import Entity.EnumInfo
import qualified Entity.EnumTypeName as ET
import Entity.GlobalName
import Entity.Hint

data Context = Context
  { registerTopLevelFunc :: Hint -> DD.DefiniteDescription -> Arity -> IO (),
    registerEnum :: Hint -> ET.EnumTypeName -> [EnumValue] -> IO (),
    registerResource :: Hint -> DD.DefiniteDescription -> IO (),
    registerData :: Hint -> DD.DefiniteDescription -> Arity -> [DD.DefiniteDescription] -> IO (),
    lookup :: DD.DefiniteDescription -> IO (Maybe GlobalName)
  }

newtype Config = Config
  { throwCtx :: Throw.Context
  }
