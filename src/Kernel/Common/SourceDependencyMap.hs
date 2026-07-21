module Kernel.Common.SourceDependencyMap (SourceDependencyMap) where

import Data.HashMap.Strict qualified as Map
import Path

type SourceDependencyMap =
  Map.HashMap (Path Abs File) [Path Abs File]
