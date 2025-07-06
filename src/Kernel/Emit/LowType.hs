module Kernel.Emit.LowType (emitLowType) where

import Data.ByteString.Builder
import Kernel.Emit.Builder
import Kernel.Emit.PrimType
import Language.Common.LowType qualified as LT

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
    LT.Pointer ->
      "ptr"
    LT.Void ->
      "void"
    LT.VarArgs ->
      "..."
