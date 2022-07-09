module Entity.EnumTypeName
  ( EnumTypeName (..),
  )
where

import Data.Binary
import qualified Data.Text as T
import GHC.Generics

newtype EnumTypeName = EnumTypeName {reify :: T.Text}
  deriving (Semigroup, Monoid, Show, Generic, Eq, Ord)

instance Binary EnumTypeName
