module Entity.NameDependenceMap (NameDependenceMap) where

import Data.HashMap.Strict qualified as Map
import Entity.TopNameMap
import Path

type NameDependenceMap =
  Map.HashMap (Path Abs File) TopNameMap
