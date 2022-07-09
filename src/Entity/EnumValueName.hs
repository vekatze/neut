module Entity.EnumValueName
  ( EnumValueName (..),
    new,
  )
where

import Data.Binary
import qualified Data.Text as T
import Entity.Const
import qualified Entity.EnumTypeName as ET
import GHC.Generics

newtype EnumValueName = EnumValueName {reify :: T.Text}
  deriving (Semigroup, Monoid, Generic, Eq, Ord)

instance Binary EnumValueName

instance Show EnumValueName where
  show v = T.unpack $ reify v

new :: ET.EnumTypeName -> T.Text -> EnumValueName
new (ET.EnumTypeName enumTypeName) valueBaseName =
  EnumValueName $ enumTypeName <> nsSep <> valueBaseName
