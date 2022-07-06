module Entity.Target where

import Data.Hashable
import qualified Data.Text as T
import GHC.Generics (Generic)

newtype Target = Target {extract :: T.Text}
  deriving (Show, Eq, Generic)

instance Hashable Target
