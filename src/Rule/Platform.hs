module Rule.Platform
  ( Platform (..),
    reify,
  )
where

import Data.Text qualified as T
import Rule.Arch qualified as Arch
import Rule.OS qualified as OS

data Platform = Platform
  { os :: OS.OS,
    arch :: Arch.Arch
  }

reify :: Platform -> T.Text
reify target =
  Arch.reify (arch target) <> "-" <> OS.reify (os target)
