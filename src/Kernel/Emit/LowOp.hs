module Kernel.Emit.LowOp
  ( Handle,
    new,
    emitLowOp,
  )
where

import Data.ByteString.Builder
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Kernel.Common.Allocator (AllocatorSpec (..))
import Kernel.Emit.LowType
import Kernel.Emit.LowValue
import Kernel.Emit.PrimType
import Language.Common.DataSize (DataSize)
import Language.Common.LowType qualified as LT
import Language.Common.PrimNumSize
import Language.Common.SlotSize
import Language.Common.PrimOp
import Language.Common.PrimType qualified as PT
import Language.LowComp.LowComp qualified as LC

data Handle = Handle
  { baseSize :: DataSize,
    intType :: LT.LowType,
    stackSlotAlignment :: Int,
    allocatorSpec :: AllocatorSpec
  }

new :: DataSize -> AllocatorSpec -> Handle
new baseSize allocatorSpec = do
  let intType = LT.PrimNum $ PT.Int $ dataSizeToIntSize baseSize
  let stackSlotAlignment = slotByteSize
  Handle {..}

emitLowOp :: Handle -> LC.Op -> Builder
emitLowOp ax lowOp = do
  let emitValue' = emitValue (baseSize ax)
  case lowOp of
    LC.Call isPure codType d ds -> do
      let renderedArgs = showInternalArgs (baseSize ax) ds
      let attributes = if isPure then ["nounwind willreturn memory(read)"] else []
      unwordsL $ ["call fastcc", emitInternalReturnType codType, emitValue' d <> renderedArgs] <> attributes
    LC.MagicCall funcType d ds ->
      unwordsL ["call", emitLowType funcType, emitValue' d <> showArgs (baseSize ax) ds]
    LC.GetElementPtr (basePtr, n) is ->
      unwordsL
        [ "getelementptr",
          emitLowType n <> ",",
          emitLowType LT.Pointer,
          emitValue' basePtr <> ",",
          showIndex (baseSize ax) is
        ]
    LC.Bitcast d fromType toType ->
      emitConvOp (baseSize ax) "bitcast" d fromType toType
    LC.IntToPointer d fromType ->
      emitConvOp (baseSize ax) "inttoptr" d fromType LT.Pointer
    LC.PointerToInt d toType ->
      emitConvOp (baseSize ax) "ptrtoint" d LT.Pointer toType
    LC.Load d lowType ->
      unwordsL
        [ "load",
          emitLowType lowType <> ",",
          emitLowType LT.Pointer,
          emitValue' d
        ]
    LC.Store t d1 d2 ->
      unwordsL
        [ "store",
          emitLowType t,
          emitValue' d1 <> ",",
          emitLowType LT.Pointer,
          emitValue' d2
        ]
    LC.StackAlloc stackAllocInfo -> do
      unwordsL
        [ "alloca",
          emitLowType (LC.stackElemType stackAllocInfo) <> ",",
          emitLowType (LC.stackIndexType stackAllocInfo),
          emitStackSize (baseSize ax) (LC.stackSize stackAllocInfo)
            <> ", align "
            <> intDec (stackSlotAlignment ax)
        ]
    LC.StackLifetimeStart {} ->
      ""
    LC.StackLifetimeEnd {} ->
      ""
    LC.Calloc num size -> do
      unwordsL
        [ "call",
          "ptr",
          "@"
            <> TE.encodeUtf8Builder (callocName $ allocatorSpec ax)
            <> "("
            <> emitLowType (intType ax)
            <> " "
            <> emitValue' num
            <> ", "
            <> emitLowType (intType ax)
            <> " "
            <> emitValue' size
            <> ")"
        ]
    LC.Alloc size _ -> do
      unwordsL
        [ "call",
          "ptr",
          "@"
            <> TE.encodeUtf8Builder (mallocName $ allocatorSpec ax)
            <> "("
            <> emitLowType (intType ax)
            <> " "
            <> emitAllocSize (baseSize ax) size
            <> ")"
        ]
    LC.Realloc ptr size -> do
      unwordsL
        [ "call",
          "ptr",
          "@"
            <> TE.encodeUtf8Builder (reallocName $ allocatorSpec ax)
            <> "(ptr "
            <> emitValue' ptr
            <> ", "
            <> emitLowType (intType ax)
            <> " "
            <> emitValue' size
            <> ")"
        ]
    LC.Free d _ _ -> do
      unwordsL
        [ "call",
          "void",
          "@"
            <> TE.encodeUtf8Builder (freeName $ allocatorSpec ax)
            <> "(ptr "
            <> emitValue' d
            <> ")"
        ]
    LC.PrimOp op args -> do
      case op of
        PrimUnaryOp name dom _ -> do
          let name' = TE.encodeUtf8Builder (T.pack $ show name)
          case args of
            [arg] ->
              emitUnaryOp (baseSize ax) dom name' arg
            _ ->
              error "Kernel.Emit.LowOp.emitLowOp.PrimUnaryOp"
        PrimBinaryOp name dom _ -> do
          let name' = TE.encodeUtf8Builder (T.pack $ show name)
          case args of
            [arg1, arg2] ->
              emitBinaryOp (baseSize ax) dom name' arg1 arg2
            _ ->
              error "Kernel.Emit.LowOp.emitLowOp.PrimBinaryOp"
        PrimCmpOp name dom _ -> do
          let name' = TE.encodeUtf8Builder (T.pack $ show name)
          case args of
            [arg1, arg2] ->
              emitBinaryOp (baseSize ax) dom name' arg1 arg2
            _ ->
              error "Kernel.Emit.LowOp.emitLowOp.PrimCmpOp"
        PrimConvOp name dom cod -> do
          let name' = TE.encodeUtf8Builder (T.pack $ show name)
          case args of
            [arg] ->
              emitConvOp (baseSize ax) name' arg (LT.PrimNum dom) (LT.PrimNum cod)
            _ ->
              error "Kernel.Emit.LowOp.emitLowOp.PrimConvOp"

emitStackSize :: DataSize -> Either Integer LC.Value -> Builder
emitStackSize baseSize stackSize =
  case stackSize of
    Left knownSize ->
      integerDec knownSize
    Right runtimeSize ->
      emitValue baseSize runtimeSize

emitAllocSize :: DataSize -> Either Integer LC.Value -> Builder
emitAllocSize baseSize allocSize =
  case allocSize of
    Left knownSize ->
      integerDec knownSize
    Right runtimeSize ->
      emitValue baseSize runtimeSize

emitUnaryOp :: DataSize -> PT.PrimType -> Builder -> LC.Value -> Builder
emitUnaryOp baseSize t inst d =
  unwordsL [inst, emitPrimType t, emitValue baseSize d]

emitBinaryOp :: DataSize -> PT.PrimType -> Builder -> LC.Value -> LC.Value -> Builder
emitBinaryOp baseSize t inst d1 d2 =
  unwordsL [inst, emitPrimType t, emitValue baseSize d1 <> ",", emitValue baseSize d2]

emitConvOp :: DataSize -> Builder -> LC.Value -> LT.LowType -> LT.LowType -> Builder
emitConvOp baseSize cast d dom cod =
  unwordsL [cast, emitLowType dom, emitValue baseSize d, "to", emitLowType cod]

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

showIndex :: DataSize -> [(LC.Value, LT.LowType)] -> Builder
showIndex baseSize idxList =
  case idxList of
    [] ->
      ""
    [(d, t)] ->
      emitLowType t <> " " <> emitValue baseSize d
    ((d, t) : dts) ->
      showIndex baseSize [(d, t)] <> ", " <> showIndex baseSize dts
