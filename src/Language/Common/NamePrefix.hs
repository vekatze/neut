module Language.Common.NamePrefix
  ( NamePrefix (..),
  )
where

import Language.Common.BaseName qualified as BN

data NamePrefix modulePath
  = ModulePrefix modulePath
  | SourcePrefix modulePath [BN.BaseName]
  | BodyPrefix modulePath [BN.BaseName] [BN.BaseName]
  deriving (Eq, Show)
