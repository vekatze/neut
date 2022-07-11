module Entity.ModuleChecksum where

import Data.Binary
import Data.Hashable
import qualified Data.Text as T
import GHC.Generics

newtype ModuleChecksum
  = ModuleChecksum T.Text
  deriving (Show, Ord, Eq, Generic)

instance Binary ModuleChecksum

instance Hashable ModuleChecksum

reify :: ModuleChecksum -> T.Text
reify (ModuleChecksum checksum) =
  checksum
