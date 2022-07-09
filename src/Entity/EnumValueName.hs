module Entity.EnumValueName
  ( EnumValueName (..),
    new,
  )
where

import Data.Binary
import qualified Data.Text as T
import Entity.Const
import Entity.EnumTypeName
import GHC.Generics

newtype EnumValueName = EnumValueName {reify :: T.Text}
  deriving (Semigroup, Monoid, Show, Generic)

instance Binary EnumValueName

new :: EnumTypeName -> T.Text -> EnumValueName
new (EnumTypeName enumTypeName) valueBaseName =
  EnumValueName $ enumTypeName <> nsSep <> valueBaseName
