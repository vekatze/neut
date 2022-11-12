module Entity.PrimOp.FromText (fromDefiniteDescription) where

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Entity.BaseName as BN
import qualified Entity.DefiniteDescription as DD
import qualified Entity.LocalLocator as LL
import Entity.PrimNumSize
import Entity.PrimNumSize.ToInt
import qualified Entity.PrimNumType as PNT
import qualified Entity.PrimNumType.FromText as PrimNum
import Entity.PrimOp
import Entity.PrimOp.OpSet
import qualified Entity.StrictGlobalLocator as SGL

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
    Just primNum@(PNT.Float _) <- PrimNum.fromText typeStr =
      Just $ PrimOp "fneg" [primNum] primNum
  | Just (convOpStr, rest) <- breakOnMaybe "-" name,
    Just (domTypeStr, codTypeStr) <- breakOnMaybe "-" rest,
    Just domType <- PrimNum.fromText domTypeStr,
    Just codType <- PrimNum.fromText codTypeStr,
    isValidConvOp convOpStr domType codType =
      Just $ PrimOp convOpStr [domType] codType
  | Just (opStr, typeStr) <- breakOnMaybe "-" name =
      case PrimNum.fromText typeStr of
        Just primNum@(PNT.Int _)
          | asLowICmpMaybe opStr ->
              Just $ PrimOp ("icmp " <> opStr) [primNum, primNum] (PNT.Int $ IntSize 1)
        Just primNum@(PNT.Float _)
          | asLowFCmpMaybe opStr ->
              Just $ PrimOp ("fcmp " <> opStr) [primNum, primNum] (PNT.Int $ IntSize 1)
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

isValidConvOp :: T.Text -> PNT.PrimNumType -> PNT.PrimNumType -> Bool
isValidConvOp name domType codType =
  case name of
    "trunc"
      | PNT.Int i1 <- domType,
        PNT.Int i2 <- codType ->
          intSizeToInt i1 > intSizeToInt i2
    "zext"
      | PNT.Int i1 <- domType,
        PNT.Int i2 <- codType ->
          intSizeToInt i1 < intSizeToInt i2
    "sext"
      | PNT.Int i1 <- domType,
        PNT.Int i2 <- codType ->
          intSizeToInt i1 < intSizeToInt i2
    "fptrunc"
      | PNT.Float size1 <- domType,
        PNT.Float size2 <- codType ->
          floatSizeToInt size1 > floatSizeToInt size2
    "fpext"
      | PNT.Float size1 <- domType,
        PNT.Float size2 <- codType ->
          floatSizeToInt size1 < floatSizeToInt size2
    "fptoui"
      | PNT.Float _ <- domType,
        PNT.Int _ <- codType ->
          True
    "fptosi"
      | PNT.Float _ <- domType,
        PNT.Int _ <- codType ->
          True
    "uitofp"
      | PNT.Int _ <- domType,
        PNT.Float _ <- codType ->
          True
    "sitofp"
      | PNT.Int _ <- domType,
        PNT.Float _ <- codType ->
          True
    _ ->
      False

asLowBinaryOpMaybe' :: T.Text -> PNT.PrimNumType -> Bool
asLowBinaryOpMaybe' name primNum =
  case primNum of
    PNT.Int _ ->
      S.member name intBinaryOpSet
    PNT.Float _ ->
      S.member name floatBinaryOpSet

asLowICmpMaybe :: T.Text -> Bool
asLowICmpMaybe name =
  S.member name intCmpOpSet

asLowFCmpMaybe :: T.Text -> Bool
asLowFCmpMaybe name =
  S.member name floatCmpOpSet
