module Entity.Artifact (ArtifactTime (..)) where

import Data.Time

data ArtifactTime = ArtifactTime
  { cacheTime :: Maybe UTCTime,
    llvmTime :: Maybe UTCTime,
    asmTime :: Maybe UTCTime,
    objectTime :: Maybe UTCTime
  }
  deriving (Show)
