module Language.Common.Rule.GlobalLocatorAlias (GlobalLocatorAlias (..)) where

import Data.Binary
import Data.Hashable
import GHC.Generics (Generic)
import Language.Common.Rule.BaseName qualified as BN

newtype GlobalLocatorAlias = GlobalLocatorAlias {reify :: BN.BaseName}
  deriving (Eq, Show, Generic)

instance Hashable GlobalLocatorAlias

instance Binary GlobalLocatorAlias
