module Language.Common.LowType (LowType (..), textType, textTypeInner) where

import Data.Binary
import GHC.Generics qualified as G
import Language.Common.DataSize (DataSize)
import Language.Common.PrimNumSize
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

textType :: DataSize -> Int -> LowType
textType baseSize len =
  Struct
    [ PrimNum $ PT.Int $ dataSizeToIntSize baseSize,
      PrimNum $ PT.Int $ dataSizeToIntSize baseSize,
      textTypeInner len
    ]

textTypeInner :: Int -> LowType
textTypeInner len =
  Array len (PrimNum $ PT.Int IntSize8)
