module Data.Spec where

import qualified Data.HashMap.Lazy as Map
import Data.Module (AliasName, Checksum)
import qualified Data.Text as T
import Path (Abs, Dir, File, Path)

newtype URL
  = URL T.Text

data Spec = Spec
  { specSourceDir :: Path Abs Dir,
    specTargetDir :: Path Abs Dir,
    specEntryPoint :: Map.HashMap T.Text (Path Abs File),
    specDependency :: Map.HashMap AliasName (URL, Checksum),
    specLocation :: Path Abs File
  }
