module Entity.Cache where

import Data.Binary
import Entity.ExportInfo
import Entity.Remark
import Entity.Stmt
import GHC.Generics

data Cache = Cache
  { stmtList :: [Stmt],
    remarkList :: [Remark],
    exportInfoList :: [ExportClause]
  }
  deriving (Generic)

instance Binary Cache
