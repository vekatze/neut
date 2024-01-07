module Entity.Cache where

import Data.Binary
import Entity.LocalVarTree qualified as LVT
import Entity.LocationTree qualified as LT
import Entity.Remark
import Entity.Stmt qualified as Stmt
import GHC.Generics

data Cache = Cache
  { stmtList :: [Stmt.Stmt],
    remarkList :: [Remark],
    locationTree :: LT.LocationTree,
    localVarTree :: LVT.LocalVarTree
  }
  deriving (Generic)

data LowCache = LowCache
  { stmtList' :: [Stmt.StrippedStmt],
    remarkList' :: [Remark],
    locationTree' :: LT.LocationTree,
    localVarTree' :: LVT.LocalVarTree
  }
  deriving (Generic)

instance Binary LowCache

compress :: Cache -> LowCache
compress cache =
  LowCache
    { stmtList' = map Stmt.compress (stmtList cache),
      remarkList' = remarkList cache,
      locationTree' = locationTree cache,
      localVarTree' = localVarTree cache
    }

extend :: LowCache -> Cache
extend cache =
  Cache
    { stmtList = map Stmt.extend (stmtList' cache),
      remarkList = remarkList' cache,
      locationTree = locationTree' cache,
      localVarTree = localVarTree' cache
    }
