module Entity.EnumValueName
  ( EnumValueName (..),
    new,
    new',
  )
where

import Data.Binary
import qualified Data.Text as T
import qualified Entity.DefiniteDescription as DD
import qualified Entity.EnumTypeName as ET
import qualified Entity.LocalLocator as LL
import GHC.Generics

newtype EnumValueName = EnumValueName {reify :: DD.DefiniteDescription}
  deriving (Generic, Eq, Ord)

instance Binary EnumValueName

instance Show EnumValueName where
  show v = T.unpack $ DD.reify $ reify v

new :: ET.EnumTypeName -> T.Text -> EnumValueName
new (ET.EnumTypeName enumTypeName) valueBaseName = do
  EnumValueName $ DD.extend enumTypeName valueBaseName

new' :: ET.EnumTypeName -> LL.LocalLocator -> EnumValueName
new' (ET.EnumTypeName enumTypeName) valueBaseName = do
  EnumValueName $ DD.extendLL enumTypeName valueBaseName
