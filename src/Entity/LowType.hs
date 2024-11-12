module Entity.LowType where

import Data.Binary
import Entity.Arch qualified as A
import Entity.ArgNum qualified as AN
import Entity.DataSize qualified as DS
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
  | VarArgs
  deriving (Eq, Ord, G.Generic)

instance Show LowType where
  show _ = "<LT>"

instance Binary LowType

toVoidPtrSeq :: AN.ArgNum -> [LowType]
toVoidPtrSeq argNum =
  map (const Pointer) [1 .. AN.reify argNum]

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

getDefaultInt :: A.Arch -> LowType
getDefaultInt arch =
  PrimNum $ PT.Int $ IntSize $ DS.reify $ A.dataSizeOf arch
