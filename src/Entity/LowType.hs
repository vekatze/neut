module Entity.LowType where

import Data.Binary
import Entity.Arity qualified as A
import Entity.PrimNumSize
import Entity.PrimType qualified as PT
import GHC.Generics qualified as G

data LowType
  = PrimNum PT.PrimType
  | Pointer
  | Array Int LowType -- [n x LOWTYPE]
  | Struct [LowType]
  | Function [LowType] LowType
  | Void
  deriving (Eq, Ord, G.Generic)

instance Show LowType where
  show _ = "<LT>"

instance Binary LowType

toVoidPtrSeq :: A.Arity -> [LowType]
toVoidPtrSeq arity =
  map (const Pointer) [1 .. A.reify arity]

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
