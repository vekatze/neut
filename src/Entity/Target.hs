module Entity.Target where

import Data.Hashable
import Data.Text qualified as T
import GHC.Generics (Generic)
import Path

data Target
  = Target T.Text
  | ZenTarget (Path Abs File)
  deriving (Show, Eq, Generic)

instance Hashable Target
