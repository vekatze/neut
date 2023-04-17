module Entity.LowComp.EmitOp (emitLowOp) where

import Data.ByteString.Builder
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Entity.Arch qualified as Arch
import Entity.LowComp qualified as LC
import Entity.LowComp.EmitValue
import Entity.LowType qualified as LT
import Entity.LowType.EmitLowType
import Entity.OS qualified as OS
import Entity.PrimNumSize
import Entity.PrimOp
import Entity.PrimType qualified as PT
import Entity.PrimType.EmitPrimType
import Entity.TargetPlatform qualified as TP

emitLowOp :: TP.TargetPlatform -> LC.Op -> Either String Builder
emitLowOp targetPlatform lowOp =
  case lowOp of
    LC.Call d ds ->
      return $ unwordsL ["call fastcc i8*", emitValue d <> showArgs ds]
    LC.GetElementPtr (basePtr, n) is ->
      return $
        unwordsL
          [ "getelementptr",
            emitLowTypeAsIfNonPtr n <> ",",
            emitLowType n,
            emitValue basePtr <> ",",
            showIndex is
          ]
    LC.Bitcast d fromType toType ->
      return $ emitConvOp "bitcast" d fromType toType
    LC.IntToPointer d fromType toType ->
      return $ emitConvOp "inttoptr" d fromType toType
    LC.PointerToInt d fromType toType ->
      return $ emitConvOp "ptrtoint" d fromType toType
    LC.Load d lowType ->
      return $
        unwordsL
          [ "load",
            emitLowType lowType <> ",",
            emitLowTypeAsIfPtr lowType,
            emitValue d
          ]
    LC.Store t d1 d2 ->
      return $
        unwordsL
          [ "store",
            emitLowType t,
            emitValue d1 <> ",",
            emitLowTypeAsIfPtr t,
            emitValue d2
          ]
    LC.Alloc d _ ->
      return $ unwordsL ["call fastcc", "i8*", "@malloc(i8* " <> emitValue d <> ")"]
    LC.Free d -> do
      return $ unwordsL ["call fastcc", "i8*", "@free(i8* " <> emitValue d <> ")"]
    LC.Syscall num ds ->
      emitSyscallOp targetPlatform num ds
    LC.PrimOp op args -> do
      case op of
        PrimUnaryOp name dom _ -> do
          let name' = TE.encodeUtf8Builder name
          return $ emitUnaryOp dom name' (head args)
        PrimBinaryOp name dom _ -> do
          let name' = TE.encodeUtf8Builder name
          return $ emitBinaryOp dom name' (head args) (args !! 1)
        PrimCmpOp name dom _ -> do
          let name' = TE.encodeUtf8Builder name
          return $ emitBinaryOp dom name' (head args) (args !! 1)
        PrimConvOp name dom cod -> do
          let name' = TE.encodeUtf8Builder name
          return $ emitConvOp name' (head args) (LT.PrimNum dom) (LT.PrimNum cod)

emitUnaryOp :: PT.PrimType -> Builder -> LC.Value -> Builder
emitUnaryOp t inst d =
  unwordsL [inst, emitPrimType t, emitValue d]

emitBinaryOp :: PT.PrimType -> Builder -> LC.Value -> LC.Value -> Builder
emitBinaryOp t inst d1 d2 =
  unwordsL [inst, emitPrimType t, emitValue d1 <> ",", emitValue d2]

emitConvOp :: Builder -> LC.Value -> LT.LowType -> LT.LowType -> Builder
emitConvOp cast d dom cod =
  unwordsL [cast, emitLowType dom, emitValue d, "to", emitLowType cod]

emitSyscallOp :: TP.TargetPlatform -> Integer -> [LC.Value] -> Either String Builder
emitSyscallOp targetPlatform num ds = do
  regList <- getRegList targetPlatform
  case TP.arch targetPlatform of
    Arch.Amd64 -> do
      let args = (LC.Int num, LT.PrimNum $ PT.Int (IntSize 64)) : map (,LT.voidPtr) ds
      let argStr = "(" <> showIndex args <> ")"
      let regStr = "\"=r" <> showRegList (take (length args) regList) <> "\""
      return $
        unwordsL ["call fastcc i8* asm sideeffect \"syscall\",", regStr, argStr]
    Arch.Arm64 -> do
      let args = (LC.Int num, LT.PrimNum $ PT.Int (IntSize 64)) : map (,LT.voidPtr) ds
      let argStr = "(" <> showIndex args <> ")"
      let regStr = "\"=r" <> showRegList (take (length args) regList) <> "\""
      return $
        unwordsL ["call fastcc i8* asm sideeffect \"svc 0\",", regStr, argStr]
    Arch.Unknown name ->
      Left $ "unsupported target architecture: " <> T.unpack name

getRegList :: TP.TargetPlatform -> Either String [Builder]
getRegList targetPlatform = do
  case (TP.arch targetPlatform, TP.os targetPlatform) of
    (Arch.Amd64, OS.Linux) ->
      return ["rax", "rdi", "rsi", "rdx", "rcx", "r8", "r9"]
    (Arch.Arm64, OS.Linux) ->
      return ["x8", "x0", "x1", "x2", "x3", "x4", "x5"]
    (Arch.Amd64, OS.Darwin) ->
      return ["rax", "rdi", "rsi", "rdx", "r10", "r8", "r9"]
    _ ->
      Left $ "unsupported target: " <> T.unpack (TP.reify targetPlatform)

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

showRegList :: [Builder] -> Builder
showRegList regList =
  case regList of
    [] ->
      ""
    (s : ss) ->
      ",{" <> s <> "}" <> showRegList ss
