module Language.Common.DataInfo
  ( DataInfo (..),
    ConsInfo (..),
    FieldHint (..),
    FieldLayout (..),
    StmtConsInfo,
    fieldLayoutSlotCount,
    headerSlotCount,
    closureSlotCount,
    dataTotalSlotCount,
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
    consArgHints :: [FieldHint],
    consArgLayouts :: [FieldLayout],
    discriminant :: D.Discriminant
  }
  deriving (Generic)

data FieldHint
  = FieldAuto
  | FieldMixed
  deriving (Show, Eq, Generic)

instance Binary FieldHint

data FieldLayout
  = LayoutDirect
  | LayoutFlattened Int
  deriving (Show, Eq, Generic)

instance Binary FieldLayout

type StmtConsInfo binder =
  (SavedHint, ConsInfo binder)

instance Binary binder => Binary (DataInfo binder)

instance Binary binder => Binary (ConsInfo binder)

fieldLayoutSlotCount :: FieldLayout -> Int
fieldLayoutSlotCount layout =
  case layout of
    LayoutDirect ->
      1
    LayoutFlattened slotCount ->
      slotCount

closureSlotCount :: Int
closureSlotCount =
  3

headerSlotCount :: [a] -> Int
headerSlotCount consInfoList =
  if length consInfoList >= 2
    then 1
    else 0

dataTotalSlotCount :: [binder] -> [ConsInfo binder] -> Int
dataTotalSlotCount dataArgs consInfoList = do
  let payloadSlotCount = foldr (max . consPayloadSlotCount) 0 consInfoList
  headerSlotCount consInfoList + length dataArgs + payloadSlotCount

consPayloadSlotCount :: ConsInfo binder -> Int
consPayloadSlotCount consInfo =
  sum $ map fieldLayoutSlotCount (consArgLayouts consInfo)
