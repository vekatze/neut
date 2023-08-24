module Entity.Platform
  ( Platform (..),
    platform,
    reify,
  )
where

import Data.Text qualified as T
import Entity.Arch qualified as Arch
import Entity.OS qualified as OS
import System.Info qualified as SI

data Platform = Platform
  { os :: OS.OS,
    arch :: Arch.Arch
  }

platform :: Platform
platform =
  Platform
    { os = OS.reflect (T.pack SI.os),
      arch = Arch.reflect (T.pack SI.arch)
    }

reify :: Platform -> T.Text
reify target =
  Arch.reify (arch target) <> "-" <> OS.reify (os target)
