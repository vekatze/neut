module Language.Term.Inline.ConstantFold
  ( evaluatePrimOp,
  )
where

import Control.Comonad.Cofree
import Data.Bits (complement, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Language.Common.PrimNumSize qualified as PNS
import Language.Common.PrimNumSize.ToInt (intSizeToInt)
import Language.Common.PrimOp (PrimOp (..))
import Language.Common.PrimOp.BinaryOp qualified as BinOp
import Language.Common.PrimOp.CmpOp qualified as CmpOp
import Language.Common.PrimOp.ConvOp qualified as ConvOp
import Language.Common.PrimOp.UnaryOp qualified as UnOp
import Language.Common.PrimType qualified as PT
import Language.Common.Rune qualified as Rune
import Language.Term.PrimValue qualified as PV
import Language.Term.Term qualified as TM
import Logger.Hint
import Numeric.Half

bitMask :: PNS.IntSize -> Integer
bitMask size =
  (1 `shiftL` intSizeToInt size) - 1

normalize :: PNS.IntSize -> Integer -> Integer
normalize size val =
  val .&. bitMask size

isNegative :: PNS.IntSize -> Integer -> Bool
isNegative size val =
  testBit val (intSizeToInt size - 1)

toUnsigned :: PNS.IntSize -> Integer -> Integer
toUnsigned size val = do
  let normalized = normalize size val
  if val < 0 && not (isNegative size normalized)
    then do
      let signBit = 1 `shiftL` (intSizeToInt size - 1)
      normalized .|. signBit
    else normalized

toSigned :: PNS.IntSize -> Integer -> Integer
toSigned size val = do
  let normalized = normalize size val
  if isNegative size normalized
    then do
      let signExtension = complement (bitMask size)
      normalized .|. signExtension
    else normalized

evaluatePrimOp :: Hint -> PrimOp -> [TM.Term] -> Maybe TM.Term
evaluatePrimOp m op args =
  case op of
    PrimBinaryOp binOp dom _ ->
      case args of
        [arg1, arg2] ->
          evaluateBinaryOp m binOp dom arg1 arg2
        _ ->
          Nothing
    PrimUnaryOp unOp dom _ ->
      case args of
        [arg] ->
          evaluateUnaryOp m unOp dom arg
        _ ->
          Nothing
    PrimCmpOp cmpOp dom _ ->
      case args of
        [arg1, arg2] ->
          evaluateCmpOp m cmpOp dom arg1 arg2
        _ ->
          Nothing
    PrimConvOp convOp dom cod ->
      case args of
        [arg] ->
          evaluateConvOp m convOp dom cod arg
        _ ->
          Nothing

evaluateBinaryOp :: Hint -> BinOp.BinaryOp -> PT.PrimType -> TM.Term -> TM.Term -> Maybe TM.Term
evaluateBinaryOp m binOp dom arg1 arg2 =
  case (dom, arg1, arg2) of
    (PT.Int size, _ :< TM.Prim (PV.Int intType1 _ val1), _ :< TM.Prim (PV.Int _ _ val2)) -> do
      result <- applyIntBinaryOp size binOp val1 val2
      return $ m :< TM.Prim (PV.Int intType1 size result)
    (PT.Float size, _ :< TM.Prim (PV.Float floatType1 _ val1), _ :< TM.Prim (PV.Float _ _ val2)) -> do
      result <- applyFloatBinaryOp size binOp val1 val2
      return $ m :< TM.Prim (PV.Float floatType1 size result)
    _ ->
      Nothing

evaluateUnaryOp :: Hint -> UnOp.UnaryOp -> PT.PrimType -> TM.Term -> Maybe TM.Term
evaluateUnaryOp m unOp dom arg =
  case (dom, arg) of
    (PT.Float size, _ :< TM.Prim (PV.Float floatType _ val)) -> do
      result <- applyFloatUnaryOp size unOp val
      return $ m :< TM.Prim (PV.Float floatType size result)
    _ ->
      Nothing

evaluateConvOp :: Hint -> ConvOp.ConvOp -> PT.PrimType -> PT.PrimType -> TM.Term -> Maybe TM.Term
evaluateConvOp m convOp dom cod arg =
  case (convOp, dom, cod, arg) of
    (ConvOp.Trunc, PT.Int size1, PT.Int size2, _ :< TM.Prim (PV.Int _ _ val)) ->
      intResult m size2 $ normalize size2 (normalize size1 val)
    (ConvOp.Zext, PT.Int size1, PT.Int size2, _ :< TM.Prim (PV.Int _ _ val)) ->
      intResult m size2 $ normalize size2 (toUnsigned size1 val)
    (ConvOp.Sext, PT.Int size1, PT.Int size2, _ :< TM.Prim (PV.Int _ _ val)) ->
      intResult m size2 $ normalize size2 (toSigned size1 val)
    (ConvOp.Fptrunc, PT.Float size1, PT.Float size2, _ :< TM.Prim (PV.Float _ _ val)) ->
      floatResult m size2 $ roundFloat size2 (roundFloat size1 val)
    (ConvOp.Fpext, PT.Float size1, PT.Float size2, _ :< TM.Prim (PV.Float _ _ val)) ->
      floatResult m size2 $ roundFloat size2 (roundFloat size1 val)
    (ConvOp.Uitofp, PT.Int size1, PT.Float size2, _ :< TM.Prim (PV.Int _ _ val)) ->
      floatResult m size2 $ roundFloat size2 (fromInteger (toUnsigned size1 val))
    (ConvOp.Sitofp, PT.Int size1, PT.Float size2, _ :< TM.Prim (PV.Int _ _ val)) ->
      floatResult m size2 $ roundFloat size2 (fromInteger (toSigned size1 val))
    (ConvOp.Fptoui, PT.Float size1, PT.Int size2, _ :< TM.Prim (PV.Float _ _ val)) -> do
      truncated <- truncateToInteger (roundFloat size1 val)
      if 0 <= truncated && truncated <= bitMask size2
        then intResult m size2 (normalize size2 truncated)
        else Nothing
    (ConvOp.Fptosi, PT.Float size1, PT.Int size2, _ :< TM.Prim (PV.Float _ _ val)) -> do
      truncated <- truncateToInteger (roundFloat size1 val)
      if negate (signBitOf size2) <= truncated && truncated < signBitOf size2
        then intResult m size2 (normalize size2 truncated)
        else Nothing
    _ ->
      Nothing

signBitOf :: PNS.IntSize -> Integer
signBitOf size =
  1 `shiftL` (intSizeToInt size - 1)

truncateToInteger :: Double -> Maybe Integer
truncateToInteger value =
  if isFiniteDouble value
    then Just $ truncate value
    else Nothing

intResult :: Hint -> PNS.IntSize -> Integer -> Maybe TM.Term
intResult m size value =
  Just $ m :< TM.Prim (PV.Int (m :< TM.PrimType (PT.Int size)) size value)

floatResult :: Hint -> PNS.FloatSize -> Double -> Maybe TM.Term
floatResult m size value =
  Just $ m :< TM.Prim (PV.Float (m :< TM.PrimType (PT.Float size)) size value)

evaluateCmpOp :: Hint -> CmpOp.CmpOp -> PT.PrimType -> TM.Term -> TM.Term -> Maybe TM.Term
evaluateCmpOp m cmpOp dom arg1 arg2 =
  case (dom, arg1, arg2) of
    (PT.Int size, _ :< TM.Prim (PV.Int _ _ val1), _ :< TM.Prim (PV.Int _ _ val2)) -> do
      result <- applyIntCmpOp size cmpOp val1 val2
      let resultInt = if result then 1 else 0
      let i1 = m :< TM.PrimType (PT.Int PNS.IntSize1)
      return $ m :< TM.Prim (PV.Int i1 PNS.IntSize1 resultInt)
    (PT.Float size, _ :< TM.Prim (PV.Float _ _ val1), _ :< TM.Prim (PV.Float _ _ val2)) -> do
      result <- applyFloatCmpOp size cmpOp val1 val2
      let resultInt = if result then 1 else 0
      let i1 = m :< TM.PrimType (PT.Int PNS.IntSize1)
      return $ m :< TM.Prim (PV.Int i1 PNS.IntSize1 resultInt)
    (PT.Rune, _ :< TM.Prim (PV.Rune val1), _ :< TM.Prim (PV.Rune val2)) -> do
      result <- applyIntCmpOp PNS.IntSize32 cmpOp (Rune.asInt val1) (Rune.asInt val2)
      let resultInt = if result then 1 else 0
      let i1 = m :< TM.PrimType (PT.Int PNS.IntSize1)
      return $ m :< TM.Prim (PV.Int i1 PNS.IntSize1 resultInt)
    _ ->
      Nothing

applyIntBinaryOp :: PNS.IntSize -> BinOp.BinaryOp -> Integer -> Integer -> Maybe Integer
applyIntBinaryOp size op val1 val2 =
  case op of
    BinOp.Add ->
      Just $ normalize size (val1 + val2)
    BinOp.Sub ->
      Just $ normalize size (val1 - val2)
    BinOp.Mul ->
      Just $ normalize size (val1 * val2)
    BinOp.SDiv ->
      divisionOf (toSigned size) size quot val1 val2
    BinOp.SRem ->
      divisionOf (toSigned size) size rem val1 val2
    BinOp.UDiv ->
      divisionOf (toUnsigned size) size quot val1 val2
    BinOp.URem ->
      divisionOf (toUnsigned size) size rem val1 val2
    BinOp.And ->
      Just $ normalize size (val1 .&. val2)
    BinOp.Or ->
      Just $ normalize size (val1 .|. val2)
    BinOp.Xor ->
      Just $ normalize size (val1 `xor` val2)
    BinOp.Shl ->
      shiftOf id size shiftL val1 val2
    BinOp.Lshr ->
      shiftOf (toUnsigned size) size shiftR val1 val2
    BinOp.Ashr ->
      shiftOf (toSigned size) size shiftR val1 val2
    _ ->
      Nothing

divisionOf ::
  (Integer -> Integer) ->
  PNS.IntSize ->
  (Integer -> Integer -> Integer) ->
  Integer ->
  Integer ->
  Maybe Integer
divisionOf interpret size f val1 val2 = do
  let divisor = interpret val2
  if divisor == 0
    then Nothing
    else Just $ normalize size (f (interpret val1) divisor)

shiftOf ::
  (Integer -> Integer) ->
  PNS.IntSize ->
  (Integer -> Int -> Integer) ->
  Integer ->
  Integer ->
  Maybe Integer
shiftOf interpret size f val1 val2 = do
  let amount = toUnsigned size val2
  if amount >= toInteger (intSizeToInt size)
    then Nothing
    else Just $ normalize size (f (interpret val1) (fromInteger amount))

applyFloatBinaryOp :: PNS.FloatSize -> BinOp.BinaryOp -> Double -> Double -> Maybe Double
applyFloatBinaryOp size op val1 val2 = do
  let val1' = roundFloat size val1
  let val2' = roundFloat size val2
  result <- applyFloatBinaryOp' op val1' val2'
  return $ roundFloat size result

applyFloatBinaryOp' :: BinOp.BinaryOp -> Double -> Double -> Maybe Double
applyFloatBinaryOp' op val1 val2 =
  case op of
    BinOp.FAdd ->
      Just (val1 + val2)
    BinOp.FSub ->
      Just (val1 - val2)
    BinOp.FMul ->
      Just (val1 * val2)
    BinOp.FDiv ->
      Just (val1 / val2)
    BinOp.FRem
      | isFiniteDouble val1 && isFiniteDouble val2 && val2 /= 0 ->
          Just $ restoreNegativeZero val1 (fmod val1 val2)
    _ ->
      Nothing

fmod :: Double -> Double -> Double
fmod val1 val2 = do
  let (mantissa1, exponent1) = decodeFloat val1
  let (mantissa2, exponent2) = decodeFloat val2
  let sharedExponent = min exponent1 exponent2
  let scaled1 = mantissa1 `shiftL` (exponent1 - sharedExponent)
  let scaled2 = mantissa2 `shiftL` (exponent2 - sharedExponent)
  encodeFloat (scaled1 `rem` scaled2) sharedExponent

applyFloatUnaryOp :: PNS.FloatSize -> UnOp.UnaryOp -> Double -> Maybe Double
applyFloatUnaryOp size op val = do
  let val' = roundFloat size val
  case op of
    UnOp.FNeg ->
      Just $ roundFloat size (-val')

roundFloat :: PNS.FloatSize -> Double -> Double
roundFloat size value =
  case size of
    PNS.FloatSize16
      | isNaN value || isInfinite value ->
          value
      | abs value >= halfOverflowThreshold ->
          if value < 0 then -1 / 0 else 1 / 0
      | otherwise -> do
          let rounded = realToFrac (realToFrac value :: Half)
          restoreNegativeZero value rounded
    PNS.FloatSize32 -> do
      let rounded = realToFrac (realToFrac value :: Float)
      restoreNegativeZero value rounded
    PNS.FloatSize64 ->
      value

restoreNegativeZero :: Double -> Double -> Double
restoreNegativeZero original rounded =
  if rounded == 0 && (original < 0 || isNegativeZero original)
    then -0.0
    else rounded

isFiniteDouble :: Double -> Bool
isFiniteDouble value =
  not (isInfinite value || isNaN value)

halfOverflowThreshold :: Double
halfOverflowThreshold =
  65520

applyIntCmpOp :: PNS.IntSize -> CmpOp.CmpOp -> Integer -> Integer -> Maybe Bool
applyIntCmpOp size op val1 val2 =
  case op of
    CmpOp.Eq ->
      Just (normalize size val1 == normalize size val2)
    CmpOp.Ne ->
      Just (normalize size val1 /= normalize size val2)
    CmpOp.SGt ->
      Just (toSigned size val1 > toSigned size val2)
    CmpOp.SGe ->
      Just (toSigned size val1 >= toSigned size val2)
    CmpOp.SLt ->
      Just (toSigned size val1 < toSigned size val2)
    CmpOp.SLe ->
      Just (toSigned size val1 <= toSigned size val2)
    CmpOp.UGt ->
      Just (toUnsigned size val1 > toUnsigned size val2)
    CmpOp.UGe ->
      Just (toUnsigned size val1 >= toUnsigned size val2)
    CmpOp.ULt ->
      Just (toUnsigned size val1 < toUnsigned size val2)
    CmpOp.ULe ->
      Just (toUnsigned size val1 <= toUnsigned size val2)
    _ ->
      Nothing

applyFloatCmpOp :: PNS.FloatSize -> CmpOp.CmpOp -> Double -> Double -> Maybe Bool
applyFloatCmpOp size op val1 val2 = do
  let val1' = roundFloat size val1
  let val2' = roundFloat size val2
  let unordered = isNaN val1' || isNaN val2'
  case op of
    CmpOp.FOEq ->
      Just (not unordered && val1' == val2')
    CmpOp.FONe ->
      Just (not unordered && val1' /= val2')
    CmpOp.FOGt ->
      Just (not unordered && val1' > val2')
    CmpOp.FOGe ->
      Just (not unordered && val1' >= val2')
    CmpOp.FOLt ->
      Just (not unordered && val1' < val2')
    CmpOp.FOLe ->
      Just (not unordered && val1' <= val2')
    CmpOp.FUEq ->
      Just (unordered || val1' == val2')
    CmpOp.FUNe ->
      Just (unordered || val1' /= val2')
    CmpOp.FUGt ->
      Just (unordered || val1' > val2')
    CmpOp.FUGe ->
      Just (unordered || val1' >= val2')
    CmpOp.FULt ->
      Just (unordered || val1' < val2')
    CmpOp.FULe ->
      Just (unordered || val1' <= val2')
    CmpOp.FOrd ->
      Just (not unordered)
    CmpOp.FUno ->
      Just unordered
    CmpOp.FTrue ->
      Just True
    CmpOp.FFalse ->
      Just False
    _ ->
      Nothing
