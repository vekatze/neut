module Language.Common.DataInfo
  ( DataInfo (..),
    ConsInfo (..),
    StmtConsInfo,
  )
where

import Data.Binary
import GHC.Generics
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Discriminant qualified as D
import Language.Common.IsConstLike
import Logger.Hint

data DataInfo binder = DataInfo
  { dataArgs :: [binder],
    consInfoList :: [ConsInfo binder]
  }
  deriving (Generic)

data ConsInfo binder = ConsInfo
  { consName :: DD.DefiniteDescription,
    isConstLike :: IsConstLike,
    consArgs :: [binder],
    discriminant :: D.Discriminant
  }
  deriving (Generic)

type StmtConsInfo binder =
  (SavedHint, ConsInfo binder)

instance Binary binder => Binary (DataInfo binder)

instance Binary binder => Binary (ConsInfo binder)
