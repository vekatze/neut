module Entity.PrimOp.FromText (fromDefiniteDescription) where

import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.DataSize qualified as DS
import Entity.DefiniteDescription qualified as DD
import Entity.LocalLocator qualified as LL
import Entity.PrimNumSize
import Entity.PrimOp
import Entity.PrimOp.BinaryOp
import Entity.PrimOp.CmpOp
import Entity.PrimOp.ConvOp qualified as Conv
import Entity.PrimOp.UnaryOp
import Entity.PrimType qualified as PT
import Entity.PrimType.FromText qualified as PT
import Entity.StrictGlobalLocator qualified as SGL

fromDefiniteDescription :: DS.DataSize -> DD.DefiniteDescription -> Maybe PrimOp
fromDefiniteDescription dataSize dd = do
  let sgl = DD.globalLocator dd
  let ll = DD.localLocator dd
  if SGL.llvmGlobalLocator /= sgl
    then Nothing
    else fromText dataSize $ BN.reify $ LL.baseName ll

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
