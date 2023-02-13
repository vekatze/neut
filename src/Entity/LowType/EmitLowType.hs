module Entity.LowType.EmitLowType where

import Data.ByteString.Builder
import Entity.Builder
import Entity.LowType qualified as LT
import Entity.PrimType.EmitPrimType

emitLowType :: LT.LowType -> Builder
emitLowType lowType =
  case lowType of
    LT.PrimNum primType ->
      emitPrimType primType
    LT.Struct ts ->
      "{" <> unwordsC (map emitLowType ts) <> "}"
    LT.Function ts t ->
      emitLowType t <> " (" <> unwordsC (map emitLowType ts) <> ")"
    LT.Array i t -> do
      "[" <> intDec i <> " x " <> emitLowType t <> "]"
    LT.Pointer t ->
      emitLowType t <> "*"

emitLowTypeAsIfPtr :: LT.LowType -> Builder
emitLowTypeAsIfPtr t =
  emitLowType t <> "*"

emitLowTypeAsIfNonPtr :: LT.LowType -> Builder
emitLowTypeAsIfNonPtr lowType =
  case lowType of
    LT.Pointer t ->
      emitLowType t
    _ ->
      emitLowType lowType

-- LT.PrimNum primNum ->
--   emitPrimType primNum
-- LT.Struct ts ->
--   "{" <> unwordsC (map emitLowType ts) <> "}"
-- LT.Function ts t ->
--   emitLowType t <> " (" <> unwordsC (map emitLowType ts) <> ")"
-- LT.Array i t -> do
--   "[" <> intDec i <> " x " <> emitLowType t <> "]"
