module Entity.UnresolvedName where

import Data.Binary
import Data.Text qualified as T
import GHC.Generics

newtype UnresolvedName = UnresolvedName {reify :: T.Text}
  deriving (Show, Eq, Ord, Generic)

instance Binary UnresolvedName
