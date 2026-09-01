module Kernel.Emit.LowValue
  ( emitValue,
    emitFloat,
    floatToBits,
    emitIdentAsVar,
    emitIdentAsLabel,
    emitIdentAsLabelVar,
    showArgs,
    showInternalArgs,
    internalArgAttributes,
  )
where

import Data.Bits
import Data.ByteString.Builder
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word
import GHC.Float
import Kernel.Emit.Builder
import Kernel.Emit.LowType (emitLowType)
import Kernel.Emit.PrimType (emitPrimType)
import Language.Common.DataSize qualified as DS
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.ExternalName qualified as EN
import Language.Common.Ident
import Language.Common.LowType qualified as LT
import Language.Common.PrimNumSize
import Language.Common.PrimNumSize.ToInt (floatSizeToInt)
import Language.Common.PrimType qualified as PT
import Language.LowComp.LowComp qualified as LC
import Numeric (showHex)
import Numeric.Half

emitValue :: DS.DataSize -> LC.Value -> Builder
emitValue baseSize lowValue =
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
        else do
          let bits = DS.reify baseSize
          let wordType = "i" <> intDec bits
          "inttoptr (" <> wordType <> " " <> integerDec (a .&. (bit bits - 1)) <> " to ptr)"
    LC.Null ->
      "null"

emitIdentAsVar :: Ident -> Builder
emitIdentAsVar (I (_, i)) =
  "v" <> intDec i

data FloatLiteral = FloatLiteral
  { floatLiteralBits :: Integer,
    floatLiteralDecimal :: Maybe Double
  }

floatLiteral :: FloatSize -> Double -> FloatLiteral
floatLiteral size x =
  case size of
    FloatSize16
      | isNaN x ->
          bitsOnly $ halfPreferredNaNBits x
      | isInfinite x || abs x >= halfOverflowThreshold ->
          bitsOnly $ halfInfinityBits x
      | otherwise -> do
          let x' = realToFrac x :: Half
          let rounded = realToFrac x' :: Double
          case () of
            _
              | isInfinite rounded ->
                  bitsOnly $ halfInfinityBits x
              | rounded == 0 && isNegativeDouble x ->
                  bitsOnly (0x8000 :: Word16)
              | otherwise ->
                  FloatLiteral {floatLiteralBits = toInteger $ getHalf x', floatLiteralDecimal = Just rounded}
    FloatSize32 -> do
      let x' = realToFrac x :: Float
      let bits = toInteger $ castFloatToWord32 x'
      if isNaN x' || isInfinite x'
        then bitsOnly bits
        else FloatLiteral {floatLiteralBits = bits, floatLiteralDecimal = Just (realToFrac x')}
    FloatSize64 -> do
      let bits = toInteger $ castDoubleToWord64 x
      if isNaN x || isInfinite x
        then bitsOnly bits
        else FloatLiteral {floatLiteralBits = bits, floatLiteralDecimal = Just x}

bitsOnly :: (Integral a) => a -> FloatLiteral
bitsOnly bits =
  FloatLiteral {floatLiteralBits = toInteger bits, floatLiteralDecimal = Nothing}

floatToBits :: FloatSize -> Double -> Integer
floatToBits size x =
  floatLiteralBits $ floatLiteral size x

emitFloat :: FloatSize -> Double -> Builder
emitFloat size x = do
  let literal = floatLiteral size x
  case floatLiteralDecimal literal of
    Just rounded ->
      doubleDec rounded
    Nothing ->
      emitFloatBitcast size $ floatLiteralBits literal

emitFloatBitcast :: FloatSize -> Integer -> Builder
emitFloatBitcast size bits = do
  let width = floatSizeToInt size
  let intType = "i" <> intDec width
  let floatType = emitPrimType $ PT.Float size
  "bitcast (" <> intType <> " " <> emitHexWord (width `div` 4) bits <> " to " <> floatType <> ")"

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

emitHexWord :: (Integral a) => Int -> a -> Builder
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

showArgs :: DS.DataSize -> [(LT.LowType, LC.Value)] -> Builder
showArgs baseSize tds =
  showLocals $ map (showArg baseSize) tds

showInternalArgs :: DS.DataSize -> [(LT.LowType, LC.Value)] -> Builder
showInternalArgs baseSize tds =
  showLocals $ map (showInternalArg baseSize) tds

showArg :: DS.DataSize -> (LT.LowType, LC.Value) -> Builder
showArg baseSize (t, d) =
  emitLowType t <> " " <> emitValue baseSize d

showInternalArg :: DS.DataSize -> (LT.LowType, LC.Value) -> Builder
showInternalArg baseSize (t, d) =
  attachAttributes (emitLowType t) (internalArgAttributes t)
    <> " "
    <> emitValue baseSize d

showLocals :: [Builder] -> Builder
showLocals ds =
  "(" <> unwordsC ds <> ")"

internalArgAttributes :: LT.LowType -> [Builder]
internalArgAttributes argType =
  case argType of
    LT.Pointer ->
      ["noundef"]
    _ ->
      []
