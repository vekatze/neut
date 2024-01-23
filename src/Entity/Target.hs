module Entity.Target where

import Data.Hashable
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import GHC.Generics (Generic)
import Path

data Target
  = Abstract AbstractTarget
  | Concrete ConcreteTarget
  deriving (Show, Eq, Generic)

data AbstractTarget
  = Foundation
  deriving (Show, Eq, Generic)

data ConcreteTarget
  = Named T.Text
  | Zen (Path Abs File)
  deriving (Show, Eq, Generic)

instance Hashable Target

instance Hashable AbstractTarget

instance Hashable ConcreteTarget

getEntryPointName :: ConcreteTarget -> BN.BaseName
getEntryPointName target =
  case target of
    Named {} ->
      BN.mainName
    Zen {} ->
      BN.zenName
