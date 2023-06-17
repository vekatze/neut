module Entity.Cache where

import Data.Binary
import Entity.Decl qualified as DE
import Entity.LocationTree qualified as LT
import Entity.Remark
import Entity.Stmt
import GHC.Generics

data Cache = Cache
  { stmtList :: [Stmt],
    remarkList :: [Remark],
    locationTree :: LT.LocationTree,
    declList :: [DE.Decl]
  }
  deriving (Generic)

instance Binary Cache
