module Entity.Cache where

import Data.Binary
import Entity.Decl qualified as DE
import Entity.LocationTree qualified as LT
import Entity.Remark
import Entity.Stmt qualified as Stmt
import GHC.Generics

data Cache = Cache
  { stmtList :: [Stmt.Stmt],
    remarkList :: [Remark],
    locationTree :: LT.LocationTree,
    declList :: [DE.Decl]
  }
  deriving (Generic)

instance Binary Cache

compress :: Cache -> Cache
compress cache =
  cache {stmtList = map Stmt.compress (stmtList cache)}
