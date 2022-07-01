module Context.Enum where

import qualified Data.Text as T
import Entity.EnumInfo
import Entity.Hint

type EnumTypeName = T.Text

type EnumValueName = T.Text

type Discriminant = Integer

data Axis = Axis
  { register :: Hint -> EnumTypeName -> [EnumItem] -> IO (),
    lookupType :: T.Text -> IO (Maybe [EnumItem]),
    lookupValue :: T.Text -> IO (Maybe (EnumTypeName, Discriminant))
  }

-- Map.HashMap T.Text (T.Text, Integer) -> T.Text -> Maybe EnumCase
