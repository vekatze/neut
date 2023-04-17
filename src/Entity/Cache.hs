module Entity.Cache where

import Data.Binary
import Entity.Remark
import Entity.Stmt
import GHC.Generics

data Cache = Cache
  { stmtList :: [Stmt],
    remarkList :: [Remark]
  }
  deriving (Generic)

instance Binary Cache
