module Kernel.Common.Artifact
  ( ArtifactTime (..),
    inject,
  )
where

import Data.Time

data ArtifactTime = ArtifactTime
  { cacheTime :: Maybe UTCTime,
    llvmTime :: Maybe UTCTime,
    objectTime :: Maybe UTCTime
  }
  deriving (Show, Ord, Eq)

inject :: UTCTime -> ArtifactTime
inject t =
  ArtifactTime
    { cacheTime = Just t,
      llvmTime = Just t,
      objectTime = Just t
    }
