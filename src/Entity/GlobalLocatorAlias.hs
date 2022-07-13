module Entity.GlobalLocatorAlias where

import Data.Binary
import Data.Hashable
import qualified Entity.BaseName as BN
import GHC.Generics (Generic)

newtype GlobalLocatorAlias = GlobalLocatorAlias {reify :: BN.BaseName}
  deriving (Eq, Show, Generic)

instance Hashable GlobalLocatorAlias

instance Binary GlobalLocatorAlias
