module Language.Common.LowType (LowType (..), slotLowType, textType, textTypeInner) where

import Data.Binary
import GHC.Generics qualified as G
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
  deriving (Eq, Ord, G.Generic)

instance Show LowType where
  show _ = "<LT>"

instance Binary LowType

slotLowType :: LowType
slotLowType =
  PrimNum slotPrimType

textType :: LowType
textType =
  Struct
    [ slotLowType,
      slotLowType,
      Pointer
    ]

textTypeInner :: Int -> LowType
textTypeInner len =
  Array len (PrimNum $ PT.Int IntSize8)
