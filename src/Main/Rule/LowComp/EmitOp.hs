module Main.Rule.LowComp.EmitOp
  ( Handle,
    new,
    emitLowOp,
  )
where

import Data.ByteString.Builder
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Language.Common.Rule.LowType qualified as LT
import Language.Common.Rule.PrimNumSize
import Language.Common.Rule.PrimOp
import Language.Common.Rule.PrimType qualified as PT
import Language.LowComp.Rule.LowComp qualified as LC
import Main.Rule.LowComp.EmitValue
import Main.Rule.LowType.EmitLowType
import Main.Rule.PrimType.EmitPrimType

data Handle = Handle
  { baseSize :: Int,
    intType :: LT.LowType
  }

new :: Int -> Handle
new baseSize = do
  let intType = LT.PrimNum $ PT.Int $ IntSize baseSize
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
          emitUnaryOp dom name' (head args)
        PrimBinaryOp name dom _ -> do
          let name' = TE.encodeUtf8Builder (T.pack $ show name)
          emitBinaryOp dom name' (head args) (args !! 1)
        PrimCmpOp name dom _ -> do
          let name' = TE.encodeUtf8Builder (T.pack $ show name)
          emitBinaryOp dom name' (head args) (args !! 1)
        PrimConvOp name dom cod -> do
          let name' = TE.encodeUtf8Builder (T.pack $ show name)
          emitConvOp name' (head args) (LT.PrimNum dom) (LT.PrimNum cod)

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
