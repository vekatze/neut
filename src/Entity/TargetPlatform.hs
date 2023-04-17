module Entity.TargetPlatform
  ( TargetPlatform (..),
    reify,
  )
where

import Data.Text qualified as T
import Entity.Arch qualified as Arch
import Entity.OS qualified as OS

data TargetPlatform = TargetPlatform
  { os :: OS.OS,
    arch :: Arch.Arch
  }

reify :: TargetPlatform -> T.Text
reify target =
  Arch.reify (arch target) <> "-" <> OS.reify (os target)
