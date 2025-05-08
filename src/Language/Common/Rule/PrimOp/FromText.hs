module Language.Common.Rule.PrimOp.FromText (fromDefiniteDescription) where

import Data.Text qualified as T
import Language.Common.Rule.DataSize qualified as DS
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.PrimNumSize
import Language.Common.Rule.PrimOp
import Language.Common.Rule.PrimOp.BinaryOp
import Language.Common.Rule.PrimOp.CmpOp
import Language.Common.Rule.PrimOp.ConvOp qualified as Conv
import Language.Common.Rule.PrimOp.UnaryOp
import Language.Common.Rule.PrimType qualified as PT
import Language.Common.Rule.PrimType.FromText qualified as PT

fromDefiniteDescription :: DS.DataSize -> DD.DefiniteDescription -> Maybe PrimOp
fromDefiniteDescription dataSize dd = do
  let sgl = DD.globalLocator dd
  let ll = DD.localLocator dd
  if DD.llvmGlobalLocator /= sgl
    then Nothing
    else fromText dataSize ll

fromText :: DS.DataSize -> T.Text -> Maybe PrimOp
fromText dataSize name
  | Just (convOpStr, rest) <- breakOnMaybe "-" name,
    Just (domTypeStr, codTypeStr) <- breakOnMaybe "-" rest,
    Just domType <- PT.fromText dataSize domTypeStr,
    Just codType <- PT.fromText dataSize codTypeStr,
    Just convOp <- Conv.asConvOp convOpStr domType codType =
      Just $ PrimConvOp convOp domType codType
  | Just (opStr, typeStr) <- breakOnMaybe "-" name,
    Just primType <- PT.fromText dataSize typeStr = do
      case primType of
        PT.Int {}
          | Just op <- asIntBinaryOp opStr ->
              return $ PrimBinaryOp op primType primType
          | Just op <- asIntCmpOp opStr ->
              return $ PrimCmpOp op primType (PT.Int $ IntSize 1)
        PT.Float {}
          | Just op <- asFloatUnaryOp opStr ->
              return $ PrimUnaryOp op primType primType
          | Just op <- asFloatBinaryOp opStr ->
              return $ PrimBinaryOp op primType primType
          | Just op <- asFloatCmpOp opStr ->
              return $ PrimCmpOp op primType (PT.Int $ IntSize 1)
        _ ->
          Nothing
  | otherwise =
      Nothing

{-# INLINE breakOnMaybe #-}
breakOnMaybe :: T.Text -> T.Text -> Maybe (T.Text, T.Text)
breakOnMaybe needle text =
  if T.null text
    then Nothing
    else do
      let (h, t) = T.breakOn needle text
      if T.null t
        then Nothing
        else return (h, T.tail t)
