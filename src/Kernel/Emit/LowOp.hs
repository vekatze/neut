module Kernel.Emit.LowOp
  ( Handle,
    new,
    emitLowOp,
  )
where

import Data.ByteString.Builder
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Kernel.Emit.LowType
import Kernel.Emit.LowValue
import Kernel.Emit.PrimType
import Language.Common.DataSize (DataSize)
import Language.Common.LowType qualified as LT
import Language.Common.PrimNumSize
import Language.Common.PrimOp
import Language.Common.PrimType qualified as PT
import Language.LowComp.LowComp qualified as LC

newtype Handle = Handle
  { intType :: LT.LowType
  }

new :: DataSize -> Handle
new baseSize = do
  let intType = LT.PrimNum $ PT.Int $ dataSizeToIntSize baseSize
  Handle {..}

emitLowOp :: Handle -> LC.Op -> Builder
emitLowOp ax lowOp =
  case lowOp of
    LC.Call codType d ds ->
      unwordsL ["call fastcc", emitLowType codType, emitValue d <> showArgs ds]
    LC.MagicCall funcType d ds ->
      unwordsL ["call", emitLowType funcType, emitValue d <> showArgs ds]
    LC.GetElementPtr (basePtr, n) is ->
      unwordsL
        [ "getelementptr",
          emitLowType n <> ",",
          emitLowType LT.Pointer,
          emitValue basePtr <> ",",
          showIndex is
        ]
    LC.Bitcast d fromType toType ->
      emitConvOp "bitcast" d fromType toType
    LC.IntToPointer d fromType toType ->
      emitConvOp "inttoptr" d fromType toType
    LC.PointerToInt d fromType toType ->
      emitConvOp "ptrtoint" d fromType toType
    LC.Load d lowType ->
      unwordsL
        [ "load",
          emitLowType lowType <> ",",
          emitLowType LT.Pointer,
          emitValue d
        ]
    LC.Store t d1 d2 ->
      unwordsL
        [ "store",
          emitLowType t,
          emitValue d1 <> ",",
          emitLowType LT.Pointer,
          emitValue d2
        ]
    LC.StackAlloc lt indexType num -> do
      unwordsL ["alloca", emitLowType lt <> ",", emitLowType indexType, emitValue num]
    LC.Alloc d _ _ -> do
      unwordsL ["call fastcc", "ptr", "@malloc(" <> emitLowType (intType ax) <> " " <> emitValue d <> ")"]
    LC.Free d _ _ -> do
      unwordsL ["call fastcc", "void", "@free(ptr " <> emitValue d <> ")"]
    LC.PrimOp op args -> do
      case op of
        PrimUnaryOp name dom _ -> do
          let name' = TE.encodeUtf8Builder (T.pack $ show name)
          case args of
            [arg] ->
              emitUnaryOp dom name' arg
            _ ->
              error "Kernel.Emit.LowOp.emitLowOp.PrimUnaryOp"
        PrimBinaryOp name dom _ -> do
          let name' = TE.encodeUtf8Builder (T.pack $ show name)
          case args of
            [arg1, arg2] ->
              emitBinaryOp dom name' arg1 arg2
            _ ->
              error "Kernel.Emit.LowOp.emitLowOp.PrimBinaryOp"
        PrimCmpOp name dom _ -> do
          let name' = TE.encodeUtf8Builder (T.pack $ show name)
          case args of
            [arg1, arg2] ->
              emitBinaryOp dom name' arg1 arg2
            _ ->
              error "Kernel.Emit.LowOp.emitLowOp.PrimCmpOp"
        PrimConvOp name dom cod -> do
          let name' = TE.encodeUtf8Builder (T.pack $ show name)
          case args of
            [arg] ->
              emitConvOp name' arg (LT.PrimNum dom) (LT.PrimNum cod)
            _ ->
              error "Kernel.Emit.LowOp.emitLowOp.PrimConvOp"

emitUnaryOp :: PT.PrimType -> Builder -> LC.Value -> Builder
emitUnaryOp t inst d =
  unwordsL [inst, emitPrimType t, emitValue d]

emitBinaryOp :: PT.PrimType -> Builder -> LC.Value -> LC.Value -> Builder
emitBinaryOp t inst d1 d2 =
  unwordsL [inst, emitPrimType t, emitValue d1 <> ",", emitValue d2]

emitConvOp :: Builder -> LC.Value -> LT.LowType -> LT.LowType -> Builder
emitConvOp cast d dom cod =
  unwordsL [cast, emitLowType dom, emitValue d, "to", emitLowType cod]

{-# INLINE unwordsL #-}
unwordsL :: [Builder] -> Builder
unwordsL strList =
  case strList of
    [] ->
      ""
    [b] ->
      b
    b : bs ->
      b <> " " <> unwordsL bs

showIndex :: [(LC.Value, LT.LowType)] -> Builder
showIndex idxList =
  case idxList of
    [] ->
      ""
    [(d, t)] ->
      emitLowType t <> " " <> emitValue d
    ((d, t) : dts) ->
      showIndex [(d, t)] <> ", " <> showIndex dts
