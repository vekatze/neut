module Entity.Target where

import Data.Hashable
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import GHC.Generics (Generic)
import Path

data Target
  = Target T.Text
  | ZenTarget (Path Abs File)
  deriving (Show, Eq, Generic)

instance Hashable Target

getEntryPointName :: Target -> BN.BaseName
getEntryPointName target =
  case target of
    ZenTarget {} ->
      BN.zenName
    Target {} ->
      BN.mainName
