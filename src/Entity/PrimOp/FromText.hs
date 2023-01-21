module Entity.PrimOp.FromText (fromDefiniteDescription) where

import Data.Set qualified as S
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.DefiniteDescription qualified as DD
import Entity.LocalLocator qualified as LL
import Entity.PrimNumSize
import Entity.PrimNumSize.ToInt
import Entity.PrimOp
import Entity.PrimOp.OpSet
import Entity.PrimType qualified as PT
import Entity.PrimType.FromText qualified as PT
import Entity.StrictGlobalLocator qualified as SGL

fromDefiniteDescription :: DD.DefiniteDescription -> Maybe PrimOp
fromDefiniteDescription dd = do
  let sgl = DD.globalLocator dd
  let ll = DD.localLocator dd
  if SGL.llvmGlobalLocator /= sgl || not (null (LL.sectionStack ll))
    then Nothing
    else fromText $ BN.reify $ LL.baseName ll

fromText :: T.Text -> Maybe PrimOp
fromText name
  | Just ("fneg", typeStr) <- breakOnMaybe "-" name,
    Just primNum@(PT.Float _) <- PT.fromText typeStr =
      Just $ PrimOp "fneg" [primNum] primNum
  | Just (convOpStr, rest) <- breakOnMaybe "-" name,
    Just (domTypeStr, codTypeStr) <- breakOnMaybe "-" rest,
    Just domType <- PT.fromText domTypeStr,
    Just codType <- PT.fromText codTypeStr,
    isValidConvOp convOpStr domType codType =
      Just $ PrimOp convOpStr [domType] codType
  | Just (opStr, typeStr) <- breakOnMaybe "-" name =
      case PT.fromText typeStr of
        Just primNum@(PT.Int _)
          | asLowICmpMaybe opStr ->
              Just $ PrimOp ("icmp " <> opStr) [primNum, primNum] (PT.Int $ IntSize 1)
        Just primNum@(PT.Float _)
          | asLowFCmpMaybe opStr ->
              Just $ PrimOp ("fcmp " <> opStr) [primNum, primNum] (PT.Int $ IntSize 1)
        Just primNum
          | asLowBinaryOpMaybe' opStr primNum ->
              Just $ PrimOp opStr [primNum, primNum] primNum
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

isValidConvOp :: T.Text -> PT.PrimType -> PT.PrimType -> Bool
isValidConvOp name domType codType =
  case name of
    "trunc"
      | PT.Int i1 <- domType,
        PT.Int i2 <- codType ->
          intSizeToInt i1 > intSizeToInt i2
    "zext"
      | PT.Int i1 <- domType,
        PT.Int i2 <- codType ->
          intSizeToInt i1 < intSizeToInt i2
    "sext"
      | PT.Int i1 <- domType,
        PT.Int i2 <- codType ->
          intSizeToInt i1 < intSizeToInt i2
    "fptrunc"
      | PT.Float size1 <- domType,
        PT.Float size2 <- codType ->
          floatSizeToInt size1 > floatSizeToInt size2
    "fpext"
      | PT.Float size1 <- domType,
        PT.Float size2 <- codType ->
          floatSizeToInt size1 < floatSizeToInt size2
    "fptoui"
      | PT.Float _ <- domType,
        PT.Int _ <- codType ->
          True
    "fptosi"
      | PT.Float _ <- domType,
        PT.Int _ <- codType ->
          True
    "uitofp"
      | PT.Int _ <- domType,
        PT.Float _ <- codType ->
          True
    "sitofp"
      | PT.Int _ <- domType,
        PT.Float _ <- codType ->
          True
    _ ->
      False

asLowBinaryOpMaybe' :: T.Text -> PT.PrimType -> Bool
asLowBinaryOpMaybe' name primNum =
  case primNum of
    PT.Int _ ->
      S.member name intBinaryOpSet
    PT.Float _ ->
      S.member name floatBinaryOpSet

asLowICmpMaybe :: T.Text -> Bool
asLowICmpMaybe name =
  S.member name intCmpOpSet

asLowFCmpMaybe :: T.Text -> Bool
asLowFCmpMaybe name =
  S.member name floatCmpOpSet
