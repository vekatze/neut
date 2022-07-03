module Entity.Target
  ( Target (..),
    platform,
  )
where

data Target = Target
  { os :: String,
    arch :: String
  }

platform :: Target -> String
platform target =
  arch target <> "-" <> os target
