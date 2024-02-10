module Entity.Artifact
  ( ArtifactTime (..),
    empty,
  )
where

import Data.Time

data ArtifactTime = ArtifactTime
  { cacheTime :: Maybe UTCTime,
    llvmTime :: Maybe UTCTime,
    objectTime :: Maybe UTCTime
  }
  deriving (Show)

empty :: ArtifactTime
empty =
  ArtifactTime
    { cacheTime = Nothing,
      llvmTime = Nothing,
      objectTime = Nothing
    }
