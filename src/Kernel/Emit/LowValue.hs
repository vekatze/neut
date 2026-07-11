module Kernel.Emit.LowValue
  ( emitValue,
    emitIdentAsVar,
    emitIdentAsLabel,
    emitIdentAsLabelVar,
    showArgs,
    showArgsWithSRet,
    showFuncArgs,
    showFuncArgsWithSRet,
  )
where

import Data.ByteString.Builder
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word
import GHC.Float
import Kernel.Emit.Builder
import Kernel.Emit.LowType (emitLowType)
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ExternalName qualified as EN
import Language.Common.Ident
import Language.Common.LowType qualified as LT
import Language.Common.PrimNumSize
import Language.LowComp.LowComp qualified as LC
import Numeric (showHex)
import Numeric.Half

emitValue :: LC.Value -> Builder
emitValue lowValue =
  case lowValue of
    LC.VarLocal x ->
      "%" <> emitIdentAsVar x
    LC.VarGlobal globalName ->
      "@" <> DD.toBuilder globalName
    LC.VarExternal extName ->
      "@" <> EN.toBuilder extName
    LC.VarTextName textName ->
      "@" <> TE.encodeUtf8Builder ("\"" <> textName <> "\"")
    LC.Int i ->
      integerDec i
    LC.Float size x ->
      emitFloat size x
    LC.Address a ->
      if a == 0
        then "null"
        else "inttoptr (i64 " <> integerDec a <> " to ptr)"
    LC.Null ->
      "null"

emitIdentAsVar :: Ident -> Builder
emitIdentAsVar (I (_, i)) =
  "v" <> intDec i

emitFloat :: FloatSize -> Double -> Builder
emitFloat size x =
  case size of
    FloatSize16
      | isNaN x ->
          emitFloatBitcast "i16" 4 (halfPreferredNaNBits x) "half"
      | isInfinite x || abs x >= halfOverflowThreshold ->
          emitFloatBitcast "i16" 4 (halfInfinityBits x) "half"
      | otherwise -> do
          let x' = realToFrac x :: Half
          let rounded = realToFrac x' :: Double
          case () of
            _
              | isInfinite rounded ->
                  emitFloatBitcast "i16" 4 (halfInfinityBits x) "half"
              | rounded == 0 && isNegativeDouble x ->
                  emitFloatBitcast "i16" 4 (0x8000 :: Word16) "half"
              | otherwise ->
                  doubleDec rounded
    FloatSize32 -> do
      let x' = realToFrac x :: Float
      if isNaN x' || isInfinite x'
        then emitFloatBitcast "i32" 8 (castFloatToWord32 x') "float"
        else doubleDec (realToFrac x')
    FloatSize64 ->
      if isNaN x || isInfinite x
        then emitFloatBitcast "i64" 16 (castDoubleToWord64 x) "double"
        else doubleDec x

emitFloatBitcast :: Integral a => Builder -> Int -> a -> Builder -> Builder
emitFloatBitcast intType width bits floatType =
  "bitcast (" <> intType <> " " <> emitHexWord width bits <> " to " <> floatType <> ")"

halfPreferredNaNBits :: Double -> Word16
halfPreferredNaNBits x =
  if isNegativeDouble x
    then 0xfe00
    else 0x7e00

halfInfinityBits :: Double -> Word16
halfInfinityBits x =
  if x < 0
    then 0xfc00
    else 0x7c00

isNegativeDouble :: Double -> Bool
isNegativeDouble x =
  castDoubleToWord64 x >= 0x8000000000000000

halfOverflowThreshold :: Double
halfOverflowThreshold =
  65520

emitHexWord :: Integral a => Int -> a -> Builder
emitHexWord width bits = do
  let hex = showHex bits ""
  let padding = replicate (width - length hex) '0'
  string8 $ "u0x" <> padding <> hex

emitIdentAsLabel :: Ident -> Builder
emitIdentAsLabel (I (name, i)) =
  case T.stripPrefix "switch-label:" name of
    Just labelName ->
      TE.encodeUtf8Builder labelName
    Nothing ->
      "L" <> intDec i

emitIdentAsLabelVar :: Ident -> Builder
emitIdentAsLabelVar x =
  "%" <> emitIdentAsLabel x

showArgs :: [(LT.LowType, LC.Value)] -> Builder
showArgs tds =
  showLocals $ map showArg tds

showArgsWithSRet :: [(LT.LowType, LC.Value)] -> Builder
showArgsWithSRet tds =
  showLocals $ zipWith (curry showArgWithSRet) [0 ..] tds

showArg :: (LT.LowType, LC.Value) -> Builder
showArg (t, d) =
  emitLowType t <> " " <> emitValue d

showArgWithSRet :: (Int, (LT.LowType, LC.Value)) -> Builder
showArgWithSRet (i, (t, d))
  | i == 0 && t == LT.Pointer =
      emitLowType t <> " sret(ptr) " <> emitValue d
  | otherwise =
      showArg (t, d)

showLocals :: [Builder] -> Builder
showLocals ds =
  "(" <> unwordsC ds <> ")"

showFuncArgs :: [Builder] -> Builder
showFuncArgs ds =
  "(" <> unwordsC (map ("ptr " <>) ds) <> ")"

showFuncArgsWithSRet :: [Builder] -> Builder
showFuncArgsWithSRet ds =
  showLocals $ zipWith (curry emitFuncArgWithSRet) [0 ..] ds

emitFuncArgWithSRet :: (Int, Builder) -> Builder
emitFuncArgWithSRet (i, d)
  | i == 0 =
      "ptr sret(ptr) " <> d
  | otherwise =
      "ptr " <> d
