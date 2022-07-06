module Entity.TargetPlatform
  ( TargetPlatform (..),
    platform,
  )
where

data TargetPlatform = TargetPlatform
  { os :: String,
    arch :: String
  }

platform :: TargetPlatform -> String
platform target =
  arch target <> "-" <> os target
