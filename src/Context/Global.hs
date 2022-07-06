module Context.Global where

import qualified Context.Throw as Throw
import qualified Data.Text as T
import Entity.EnumInfo
import Entity.GlobalName
import Entity.Hint

data Axis = Axis
  { registerTopLevelFunc :: Hint -> T.Text -> IO (),
    registerEnum :: Hint -> EnumTypeName -> [EnumItem] -> IO (),
    lookup :: T.Text -> IO (Maybe GlobalName)
  }

newtype Config = Config
  { throwCtx :: Throw.Context
  }
