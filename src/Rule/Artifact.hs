module Rule.Artifact
  ( ArtifactTime (..),
    empty,
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

empty :: ArtifactTime
empty =
  ArtifactTime
    { cacheTime = Nothing,
      llvmTime = Nothing,
      objectTime = Nothing
    }

inject :: UTCTime -> ArtifactTime
inject t =
  ArtifactTime
    { cacheTime = Just t,
      llvmTime = Just t,
      objectTime = Just t
    }
