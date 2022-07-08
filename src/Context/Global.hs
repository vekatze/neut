module Context.Global where

import qualified Context.Throw as Throw
import qualified Data.Text as T
import Entity.EnumInfo
import qualified Entity.EnumTypeName as ET
import Entity.GlobalName
import Entity.Hint

data Context = Context
  { registerTopLevelFunc :: Hint -> T.Text -> IO (),
    registerEnum :: Hint -> ET.EnumTypeName -> [EnumValue] -> IO (),
    lookup :: T.Text -> IO (Maybe GlobalName)
  }

newtype Config = Config
  { throwCtx :: Throw.Context
  }
