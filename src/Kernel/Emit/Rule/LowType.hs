module Kernel.Emit.Rule.LowType (emitLowType) where

import Data.ByteString.Builder
import Kernel.Emit.Rule.Builder
import Kernel.Emit.Rule.PrimType
import Language.Common.Rule.LowType qualified as LT

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
