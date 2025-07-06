module Kernel.Common.RuleHandle.Global.Artifact
  ( Handle (..),
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Kernel.Common.Artifact qualified as A
import Path
import Prelude hiding (lookup)

newtype Handle = Handle
  { _artifactMapRef :: IORef (Map.HashMap (Path Abs File) A.ArtifactTime)
  }
