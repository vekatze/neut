module Kernel.Common.Platform
  ( Platform (..),
    PlatformSelector (..),
    reify,
    reifySelector,
    reflectSelector,
  )
where

import Data.Hashable
import Data.Text qualified as T
import GHC.Generics (Generic)
import Kernel.Common.Arch qualified as Arch
import Kernel.Common.OS qualified as OS

data Platform = Platform
  { os :: OS.OS,
    arch :: Arch.Arch
  }

data PlatformSelector
  = SelectHost
  | SelectWasm32
  deriving (Show, Eq, Generic)

instance Hashable PlatformSelector

reify :: Platform -> T.Text
reify target =
  Arch.reify (arch target) <> "-" <> OS.reify (os target)

reifySelector :: PlatformSelector -> T.Text
reifySelector selector =
  case selector of
    SelectHost ->
      "host"
    SelectWasm32 ->
      "wasm32"

reflectSelector :: T.Text -> Maybe PlatformSelector
reflectSelector text =
  case text of
    "host" ->
      Just SelectHost
    "wasm32" ->
      Just SelectWasm32
    _ ->
      Nothing
