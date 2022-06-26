{-# LANGUAGE DeriveGeneric #-}

module Entity.LowType where

import Data.Binary
import Entity.PrimNum
import Entity.PrimNumSize
import qualified GHC.Generics as G

data LowType
  = LowTypePrimNum PrimNum
  | LowTypePointer LowType
  | LowTypeArray Int LowType -- [n x LOWTYPE]
  | LowTypeStruct [LowType]
  | LowTypeFunction [LowType] LowType
  deriving (Eq, Ord, Show, G.Generic)

instance Binary LowType

voidPtr :: LowType
voidPtr =
  LowTypePointer
    (LowTypePrimNum (PrimNumInt (IntSize 8)))
