module Main.Rule.LowType (LowType (..), textType, textTypeInner) where

import Data.Binary
import GHC.Generics qualified as G
import Main.Rule.PrimNumSize
import Main.Rule.PrimType qualified as PT

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

textType :: Int -> Int -> LowType
textType baseSize len =
  Struct
    [ PrimNum $ PT.Int $ IntSize baseSize,
      PrimNum $ PT.Int $ IntSize baseSize,
      textTypeInner len
    ]

textTypeInner :: Int -> LowType
textTypeInner len =
  Array len (PrimNum $ PT.Int $ IntSize 8)
