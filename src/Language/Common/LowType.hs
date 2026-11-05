module Language.Common.LowType (LowType (..), slotLowType, textType, textTypeInner) where

import Binary.Derive (deriveBinary)
import Language.Common.DataSize qualified as DS
import Language.Common.PrimNumSize
import Language.Common.SlotSize
import Language.Common.PrimType qualified as PT

data LowType
  = PrimNum PT.PrimType
  | Pointer
  | Array Int LowType -- [n x LOWTYPE]
  | Struct [LowType]
  | Function [LowType] LowType
  | Void
  | VarArgs
  deriving (Eq, Ord)

instance Show LowType where
  show _ = "<LT>"

slotLowType :: LowType
slotLowType =
  PrimNum slotPrimType

textType :: DS.DataSize -> LowType
textType dataSize = do
  let addressInt = PrimNum $ PT.Int $ dataSizeToIntSize dataSize
  Struct
    [ addressInt,
      addressInt,
      Pointer
    ]

textTypeInner :: Int -> LowType
textTypeInner len =
  Array len (PrimNum $ PT.Int IntSize8)

deriveBinary [''LowType]
