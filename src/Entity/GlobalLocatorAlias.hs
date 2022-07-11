module Entity.GlobalLocatorAlias where

import Data.Binary
import Data.Hashable
import qualified Data.Text as T
import GHC.Generics (Generic)

newtype GlobalLocatorAlias = GlobalLocatorAlias {reify :: T.Text}
  deriving (Eq, Show, Generic)

instance Hashable GlobalLocatorAlias

instance Binary GlobalLocatorAlias
