module Entity.Cache where

import Data.Binary
import Entity.Log
import Entity.Stmt
import GHC.Generics

data Cache = Cache
  { stmtList :: [Stmt],
    logList :: [Log]
  }
  deriving (Generic)

instance Binary Cache
