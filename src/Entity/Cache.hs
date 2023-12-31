module Entity.Cache where

import Data.Binary
import Entity.Foreign qualified as F
import Entity.LocationTree qualified as LT
import Entity.Remark
import Entity.Stmt qualified as Stmt
import GHC.Generics

data Cache = Cache
  { stmtList :: [Stmt.Stmt],
    remarkList :: [Remark],
    locationTree :: LT.LocationTree,
    foreignList :: [F.Foreign]
  }
  deriving (Generic)

data LowCache = LowCache
  { stmtList' :: [Stmt.StrippedStmt],
    remarkList' :: [Remark],
    locationTree' :: LT.LocationTree,
    foreignList' :: [F.Foreign]
  }
  deriving (Generic)

instance Binary LowCache

compress :: Cache -> LowCache
compress cache =
  LowCache
    { stmtList' = map Stmt.compress (stmtList cache),
      remarkList' = remarkList cache,
      locationTree' = locationTree cache,
      foreignList' = foreignList cache
    }

extend :: LowCache -> Cache
extend cache =
  Cache
    { stmtList = map Stmt.extend (stmtList' cache),
      remarkList = remarkList' cache,
      locationTree = locationTree' cache,
      foreignList = foreignList' cache
    }
