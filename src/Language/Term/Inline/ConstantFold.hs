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
import Language.Common.PrimOp.UnaryOp qualified as UnOp
import Language.Common.PrimType qualified as PT
import Language.Term.Prim qualified as P
import Language.Term.PrimValue qualified as PV
import Language.Term.Term qualified as TM
import Logger.Hint

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
    PrimConvOp {} ->
      Nothing

evaluateBinaryOp :: Hint -> BinOp.BinaryOp -> PT.PrimType -> TM.Term -> TM.Term -> Maybe TM.Term
evaluateBinaryOp m binOp dom arg1 arg2 =
  case (dom, arg1, arg2) of
    (PT.Int size, _ :< TM.Prim (P.Value (PV.Int intType1 _ val1)), _ :< TM.Prim (P.Value (PV.Int _ _ val2))) -> do
      result <- applyIntBinaryOp size binOp val1 val2
      return $ m :< TM.Prim (P.Value (PV.Int intType1 size result))
    (PT.Float size, _ :< TM.Prim (P.Value (PV.Float floatType1 _ val1)), _ :< TM.Prim (P.Value (PV.Float _ _ val2))) -> do
      result <- applyFloatBinaryOp binOp val1 val2
      return $ m :< TM.Prim (P.Value (PV.Float floatType1 size result))
    _ ->
      Nothing

evaluateUnaryOp :: Hint -> UnOp.UnaryOp -> PT.PrimType -> TM.Term -> Maybe TM.Term
evaluateUnaryOp m unOp dom arg =
  case (dom, arg) of
    (PT.Float size, _ :< TM.Prim (P.Value (PV.Float floatType _ val))) -> do
      result <- applyFloatUnaryOp unOp val
      return $ m :< TM.Prim (P.Value (PV.Float floatType size result))
    _ ->
      Nothing

evaluateCmpOp :: Hint -> CmpOp.CmpOp -> PT.PrimType -> TM.Term -> TM.Term -> Maybe TM.Term
evaluateCmpOp m cmpOp dom arg1 arg2 =
  case (dom, arg1, arg2) of
    (PT.Int size, _ :< TM.Prim (P.Value (PV.Int _ _ val1)), _ :< TM.Prim (P.Value (PV.Int _ _ val2))) -> do
      result <- applyIntCmpOp size cmpOp val1 val2
      let resultInt = if result then 1 else 0
      let i1 = m :< TM.Prim (P.Type (PT.Int PNS.IntSize1))
      return $ m :< TM.Prim (P.Value (PV.Int i1 PNS.IntSize1 resultInt))
    (PT.Float _, _ :< TM.Prim (P.Value (PV.Float _ _ val1)), _ :< TM.Prim (P.Value (PV.Float _ _ val2))) -> do
      result <- applyFloatCmpOp cmpOp val1 val2
      let resultInt = if result then 1 else 0
      let i1 = m :< TM.Prim (P.Type (PT.Int PNS.IntSize1))
      return $ m :< TM.Prim (P.Value (PV.Int i1 PNS.IntSize1 resultInt))
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
    BinOp.SDiv
      | val2 /= 0 ->
          Just $ normalize size (val1 `div` val2)
      | otherwise ->
          Nothing
    BinOp.SRem
      | val2 /= 0 ->
          Just $ normalize size (val1 `rem` val2)
      | otherwise ->
          Nothing
    BinOp.UDiv
      | val2 /= 0 -> do
          let uval1 = toUnsigned size val1
          let uval2 = toUnsigned size val2
          Just $ normalize size (uval1 `div` uval2)
      | otherwise ->
          Nothing
    BinOp.URem
      | val2 /= 0 -> do
          let uval1 = toUnsigned size val1
          let uval2 = toUnsigned size val2
          Just $ normalize size (uval1 `rem` uval2)
      | otherwise ->
          Nothing
    BinOp.And ->
      Just $ normalize size (val1 .&. val2)
    BinOp.Or ->
      Just $ normalize size (val1 .|. val2)
    BinOp.Xor ->
      Just $ normalize size (val1 `xor` val2)
    BinOp.Shl ->
      Just $ normalize size (val1 `shiftL` fromIntegral val2)
    BinOp.Lshr -> do
      let uval = toUnsigned size val1
      Just $ normalize size (uval `shiftR` fromIntegral val2)
    BinOp.Ashr -> do
      let shiftAmount = fromIntegral val2
      let result = val1 `shiftR` shiftAmount
      Just $ normalize size result
    _ ->
      Nothing

applyFloatBinaryOp :: BinOp.BinaryOp -> Double -> Double -> Maybe Double
applyFloatBinaryOp op val1 val2 =
  case op of
    BinOp.FAdd ->
      Just (val1 + val2)
    BinOp.FSub ->
      Just (val1 - val2)
    BinOp.FMul ->
      Just (val1 * val2)
    BinOp.FDiv ->
      Just (val1 / val2)
    BinOp.FRem ->
      Just (val1 - fromIntegral (floor (val1 / val2) :: Integer) * val2)
    _ ->
      Nothing

applyFloatUnaryOp :: UnOp.UnaryOp -> Double -> Maybe Double
applyFloatUnaryOp op val =
  case op of
    UnOp.FNeg ->
      Just (-val)

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

applyFloatCmpOp :: CmpOp.CmpOp -> Double -> Double -> Maybe Bool
applyFloatCmpOp op val1 val2 =
  case op of
    CmpOp.FOEq ->
      Just (val1 == val2)
    CmpOp.FONe ->
      Just (val1 /= val2)
    CmpOp.FOGt ->
      Just (val1 > val2)
    CmpOp.FOGe ->
      Just (val1 >= val2)
    CmpOp.FOLt ->
      Just (val1 < val2)
    CmpOp.FOLe ->
      Just (val1 <= val2)
    CmpOp.FUEq ->
      Just (val1 == val2 || isNaN val1 || isNaN val2)
    CmpOp.FUNe ->
      Just (val1 /= val2 || isNaN val1 || isNaN val2)
    CmpOp.FUGt ->
      Just (val1 > val2 || isNaN val1 || isNaN val2)
    CmpOp.FUGe ->
      Just (val1 >= val2 || isNaN val1 || isNaN val2)
    CmpOp.FULt ->
      Just (val1 < val2 || isNaN val1 || isNaN val2)
    CmpOp.FULe ->
      Just (val1 <= val2 || isNaN val1 || isNaN val2)
    CmpOp.FOrd ->
      Just (not (isNaN val1 || isNaN val2))
    CmpOp.FUno ->
      Just (isNaN val1 || isNaN val2)
    CmpOp.FTrue ->
      Just True
    CmpOp.FFalse ->
      Just False
    _ ->
      Nothing
