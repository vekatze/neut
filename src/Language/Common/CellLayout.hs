module Language.Common.CellLayout
  ( FieldWidth (..),
    Chunks,
    FieldStorage (..),
    CellLayout (..),
    emptyCell,
    alignedCell,
    naturalCell,
    chunkCell,
    storageOffsets,
    storageSlots,
    storageSlotCount,
    inlineSlots,
    fieldWidthByteSize,
    fieldStorageByteSize,
    fieldStorageAlignment,
    storagesAlignment,
    storagesByteSize,
    alignUp,
    chunkSizes,
    maxChunkSize,
    widthOfByteSize,
    widthBaseLowType,
    widthOfBitSize,
    widthOfPrimType,
  )
where

import Data.Binary
import Data.List (intercalate)
import GHC.Generics
import Language.Common.BaseLowType qualified as BLT
import Language.Common.BasePrimType qualified as BPT
import Language.Common.DataSize qualified as DS
import Language.Common.PrimNumSize
import Language.Common.PrimNumSize.ToInt (floatSizeToInt, intSizeToInt)
import Language.Common.PrimType qualified as PT

data FieldWidth
  = Width8
  | Width16
  | Width32
  | Width64
  | WidthPointer
  deriving (Eq, Generic)

instance Binary FieldWidth

instance Show FieldWidth where
  show width =
    case width of
      Width8 ->
        "i8"
      Width16 ->
        "i16"
      Width32 ->
        "i32"
      Width64 ->
        "i64"
      WidthPointer ->
        "ptr"

type Chunks =
  [Int]

data FieldStorage
  = StoredDirect FieldWidth
  | StoredFlat Chunks
  deriving (Eq, Generic)

instance Binary FieldStorage

instance Show FieldStorage where
  show storage =
    case storage of
      StoredDirect width ->
        show width
      StoredFlat chunks ->
        "flat(" <> intercalate "," (map show chunks) <> ")"

data CellLayout = CellLayout
  { cellSlots :: [(Int, FieldWidth)],
    cellByteSize :: Int
  }
  deriving (Eq, Generic)

instance Binary CellLayout

instance Show CellLayout where
  show layout =
    intercalate "," (map showSlot (cellSlots layout)) <> "|" <> show (cellByteSize layout)

showSlot :: (Int, FieldWidth) -> String
showSlot (offset, width) =
  show offset <> ":" <> show width

emptyCell :: CellLayout
emptyCell =
  CellLayout {cellSlots = [], cellByteSize = 0}

alignedCell :: DS.DataSize -> [FieldStorage] -> Int -> CellLayout
alignedCell dataSize storages byteSize =
  CellLayout {cellSlots = placeSlots dataSize 0 storages, cellByteSize = byteSize}

placeSlots :: DS.DataSize -> Int -> [FieldStorage] -> [(Int, FieldWidth)]
placeSlots dataSize cursor storages =
  case storages of
    [] ->
      []
    storage : rest -> do
      let (start, end) = placeStorage dataSize cursor storage
      storageSlots start storage ++ placeSlots dataSize end rest

naturalCell :: DS.DataSize -> [FieldStorage] -> CellLayout
naturalCell dataSize storages =
  alignedCell dataSize storages $ alignUp (storagesAlignment dataSize storages) (storagesByteSize dataSize 0 storages)

chunkCell :: Chunks -> CellLayout
chunkCell chunks =
  CellLayout {cellSlots = chunkSlots 0 chunks, cellByteSize = sum chunks}

chunkSlots :: Int -> Chunks -> [(Int, FieldWidth)]
chunkSlots start chunks =
  case chunks of
    [] ->
      []
    chunk : rest ->
      (start, widthOfByteSize chunk) : chunkSlots (start + chunk) rest

storageSlots :: Int -> FieldStorage -> [(Int, FieldWidth)]
storageSlots start storage =
  case storage of
    StoredDirect width ->
      [(start, width)]
    StoredFlat chunks ->
      chunkSlots start chunks

storageSlotCount :: FieldStorage -> Int
storageSlotCount storage =
  case storage of
    StoredDirect _ ->
      1
    StoredFlat chunks ->
      length chunks

inlineSlots :: Int -> CellLayout -> [(Int, FieldWidth)]
inlineSlots start inner =
  [(start + offset, width) | (offset, width) <- cellSlots inner]

storageOffsets :: DS.DataSize -> Int -> [FieldStorage] -> [Int]
storageOffsets dataSize cursor storages =
  case storages of
    [] ->
      []
    storage : rest -> do
      let (start, end) = placeStorage dataSize cursor storage
      start : storageOffsets dataSize end rest

storagesByteSize :: DS.DataSize -> Int -> [FieldStorage] -> Int
storagesByteSize dataSize cursor storages =
  case storages of
    [] ->
      cursor
    storage : rest -> do
      let (_, end) = placeStorage dataSize cursor storage
      storagesByteSize dataSize end rest

placeStorage :: DS.DataSize -> Int -> FieldStorage -> (Int, Int)
placeStorage dataSize cursor storage = do
  let start = alignUp (fieldStorageAlignment dataSize storage) cursor
  (start, start + fieldStorageByteSize dataSize storage)

fieldWidthByteSize :: DS.DataSize -> FieldWidth -> Int
fieldWidthByteSize dataSize width =
  case width of
    Width8 ->
      1
    Width16 ->
      2
    Width32 ->
      4
    Width64 ->
      8
    WidthPointer ->
      DS.reifyBytes dataSize

fieldStorageByteSize :: DS.DataSize -> FieldStorage -> Int
fieldStorageByteSize dataSize storage =
  case storage of
    StoredDirect width ->
      fieldWidthByteSize dataSize width
    StoredFlat chunks ->
      sum chunks

fieldStorageAlignment :: DS.DataSize -> FieldStorage -> Int
fieldStorageAlignment dataSize storage =
  case storage of
    StoredDirect width ->
      fieldWidthByteSize dataSize width
    StoredFlat chunks ->
      foldr max 1 chunks

storagesAlignment :: DS.DataSize -> [FieldStorage] -> Int
storagesAlignment dataSize =
  foldr (max . fieldStorageAlignment dataSize) 1

alignUp :: Int -> Int -> Int
alignUp alignment value =
  value + rem (alignment - rem value alignment) alignment

maxChunkSize :: Int
maxChunkSize =
  8

chunkSizes :: Int -> Int -> Chunks
chunkSizes limit byteSize
  | byteSize <= 0 = []
  | byteSize >= limit = limit : chunkSizes limit (byteSize - limit)
  | otherwise = chunkSizes (limit `div` 2) byteSize

widthOfByteSize :: Int -> FieldWidth
widthOfByteSize byteSize =
  case byteSize of
    8 ->
      Width64
    4 ->
      Width32
    2 ->
      Width16
    1 ->
      Width8
    _ ->
      error $ "Language.Common.CellLayout.widthOfByteSize: " <> show byteSize

widthBaseLowType :: FieldWidth -> BLT.BaseLowType
widthBaseLowType width =
  case width of
    Width8 ->
      BLT.PrimNum $ BPT.Int $ BPT.Explicit IntSize8
    Width16 ->
      BLT.PrimNum $ BPT.Int $ BPT.Explicit IntSize16
    Width32 ->
      BLT.PrimNum $ BPT.Int $ BPT.Explicit IntSize32
    Width64 ->
      BLT.PrimNum $ BPT.Int $ BPT.Explicit IntSize64
    WidthPointer ->
      BLT.Pointer

widthOfBitSize :: Int -> FieldWidth
widthOfBitSize bitSize
  | bitSize <= 8 = Width8
  | bitSize <= 16 = Width16
  | bitSize <= 32 = Width32
  | bitSize <= 64 = Width64
  | otherwise = error $ "Language.Common.CellLayout.widthOfBitSize: " <> show bitSize

widthOfPrimType :: PT.PrimType -> FieldWidth
widthOfPrimType primType =
  case primType of
    PT.Int intSize ->
      widthOfBitSize $ intSizeToInt intSize
    PT.Float floatSize ->
      widthOfBitSize $ floatSizeToInt floatSize
    PT.Rune ->
      Width32
    PT.Text ->
      WidthPointer
    PT.Blob ->
      WidthPointer
    PT.Pointer ->
      WidthPointer
