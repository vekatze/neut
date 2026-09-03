module Language.Common.DataInfo
  ( DataInfo (..),
    ConsInfo (..),
    FieldHint (..),
    isFieldMixed,
    StmtConsInfo,
    CellShape (..),
    cellShape,
    cellShapeOf,
    consLayout,
    discriminantLoadType,
    discriminantWidth,
    dataArgStorage,
    closureLayout,
    closureAlignment,
  )
where

import Data.Binary
import GHC.Generics
import Language.Common.BaseLowType qualified as BLT
import Language.Common.CellLayout qualified as CL
import Language.Common.DataSize qualified as DS
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
    consArgLayouts :: [CL.FieldStorage],
    discriminant :: D.Discriminant
  }
  deriving (Generic)

data FieldHint
  = FieldAuto
  | FieldMixed Hint
  deriving (Show, Eq, Generic)

instance Binary FieldHint

isFieldMixed :: FieldHint -> Bool
isFieldMixed hint =
  case hint of
    FieldAuto ->
      False
    FieldMixed _ ->
      True

type StmtConsInfo binder =
  (SavedHint, ConsInfo binder)

instance Binary binder => Binary (DataInfo binder)

instance Binary binder => Binary (ConsInfo binder)

discriminantWidth :: [ConsInfo binder] -> CL.FieldWidth
discriminantWidth consInfoList =
  CL.widthOfBitSize $ discriminantBitSize $ length consInfoList

discriminantBitSize :: Int -> Int
discriminantBitSize consNum
  | consNum <= 256 = 8
  | consNum <= 65536 = 16
  | otherwise = 32

data CellShape = CellShape
  { shapeDataSize :: DS.DataSize,
    shapeHeader :: Maybe CL.FieldWidth,
    shapeDataArgCount :: Int,
    shapeAlignment :: Int,
    shapeByteSize :: Int
  }

cellShape :: DS.DataSize -> DataInfo binder -> CellShape
cellShape dataSize dataInfo =
  cellShapeOf dataSize (dataArgs dataInfo) (consInfoList dataInfo)

cellShapeOf :: DS.DataSize -> [binder] -> [ConsInfo binder] -> CellShape
cellShapeOf dataSize dataArgs consInfoList = do
  let shapeHeader = headerWidth consInfoList
  let shapeDataArgCount = length dataArgs
  let prefix = prefixStorages shapeHeader shapeDataArgCount
  let storagesList = cellStoragesList prefix consInfoList
  let alignment = foldr (max . CL.storagesAlignment dataSize) 1 storagesList
  let used = foldr (max . CL.storagesByteSize dataSize 0) 0 storagesList
  CellShape
    { shapeDataSize = dataSize,
      shapeHeader,
      shapeDataArgCount,
      shapeAlignment = alignment,
      shapeByteSize = CL.alignUp alignment used
    }

consLayout :: CellShape -> ConsInfo binder -> CL.CellLayout
consLayout shape consInfo = do
  let storages = prefixStorages (shapeHeader shape) (shapeDataArgCount shape) ++ consArgLayouts consInfo
  CL.alignedCell (shapeDataSize shape) storages (shapeByteSize shape)

discriminantLoadType :: CellShape -> BLT.BaseLowType
discriminantLoadType shape =
  maybe BLT.slot CL.widthBaseLowType $ shapeHeader shape

headerWidth :: [ConsInfo b] -> Maybe CL.FieldWidth
headerWidth consInfoList =
  if length consInfoList >= 2
    then Just $ discriminantWidth consInfoList
    else Nothing

dataArgStorage :: CL.FieldStorage
dataArgStorage =
  CL.StoredDirect CL.WidthPointer

closureLayout :: DS.DataSize -> CL.CellLayout
closureLayout dataSize =
  CL.naturalCell dataSize $ replicate 3 dataArgStorage

closureAlignment :: DS.DataSize -> Int
closureAlignment dataSize =
  CL.fieldStorageAlignment dataSize dataArgStorage

prefixStorages :: Maybe CL.FieldWidth -> Int -> [CL.FieldStorage]
prefixStorages header dataArgCount =
  maybe [] (\width -> [CL.StoredDirect width]) header ++ replicate dataArgCount dataArgStorage

cellStoragesList :: [CL.FieldStorage] -> [ConsInfo binder] -> [[CL.FieldStorage]]
cellStoragesList prefix consInfoList =
  case consInfoList of
    [] ->
      [prefix]
    _ ->
      map ((prefix ++) . consArgLayouts) consInfoList
