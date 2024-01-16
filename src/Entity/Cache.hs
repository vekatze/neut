module Entity.Cache where

import Data.Binary
import Entity.LocalVarTree qualified as LVT
import Entity.LocationTree qualified as LT
import Entity.RawImportSummary
import Entity.Remark
import Entity.Stmt qualified as Stmt
import Entity.TopCandidate (TopCandidate)
import GHC.Generics

data Cache = Cache
  { stmtList :: [Stmt.Stmt],
    remarkList :: [Remark],
    locationTree :: LT.LocationTree
  }
  deriving (Generic)

data LowCache = LowCache
  { stmtList' :: [Stmt.StrippedStmt],
    remarkList' :: [Remark],
    locationTree' :: LT.LocationTree
  }
  deriving (Generic)

instance Binary LowCache

data CompletionCache = CompletionCache
  { localVarTree :: LVT.LocalVarTree,
    topCandidate :: [TopCandidate],
    rawImportSummary :: Maybe RawImportSummary
  }
  deriving (Generic)

instance Binary CompletionCache

compress :: Cache -> LowCache
compress cache =
  LowCache
    { stmtList' = map Stmt.compress (stmtList cache),
      remarkList' = remarkList cache,
      locationTree' = locationTree cache
    }

extend :: LowCache -> Cache
extend cache =
  Cache
    { stmtList = map Stmt.extend (stmtList' cache),
      remarkList = remarkList' cache,
      locationTree = locationTree' cache
    }
