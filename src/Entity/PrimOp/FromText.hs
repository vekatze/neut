module Entity.PrimOp.FromText (fromDefiniteDescription) where

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Entity.BaseName as BN
import qualified Entity.DefiniteDescription as DD
import qualified Entity.LocalLocator as LL
import Entity.PrimNum
import qualified Entity.PrimNum.FromText as PrimNum
import Entity.PrimNumSize
import Entity.PrimNumSize.ToInt
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
    Just primNum@(PrimNumFloat _) <- PrimNum.fromText typeStr =
    Just $ PrimOp "fneg" [primNum] primNum
  | Just (convOpStr, rest) <- breakOnMaybe "-" name,
    Just (domTypeStr, codTypeStr) <- breakOnMaybe "-" rest,
    Just domType <- PrimNum.fromText domTypeStr,
    Just codType <- PrimNum.fromText codTypeStr,
    isValidConvOp convOpStr domType codType =
    Just $ PrimOp convOpStr [domType] codType
  | Just (opStr, typeStr) <- breakOnMaybe "-" name =
    case PrimNum.fromText typeStr of
      Just primNum@(PrimNumInt _)
        | asLowICmpMaybe opStr ->
          Just $ PrimOp ("icmp " <> opStr) [primNum, primNum] (PrimNumInt $ IntSize 1)
      Just primNum@(PrimNumFloat _)
        | asLowFCmpMaybe opStr ->
          Just $ PrimOp ("fcmp " <> opStr) [primNum, primNum] (PrimNumInt $ IntSize 1)
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

isValidConvOp :: T.Text -> PrimNum -> PrimNum -> Bool
isValidConvOp name domType codType =
  case name of
    "trunc"
      | PrimNumInt i1 <- domType,
        PrimNumInt i2 <- codType ->
        intSizeToInt i1 > intSizeToInt i2
    "zext"
      | PrimNumInt i1 <- domType,
        PrimNumInt i2 <- codType ->
        intSizeToInt i1 < intSizeToInt i2
    "sext"
      | PrimNumInt i1 <- domType,
        PrimNumInt i2 <- codType ->
        intSizeToInt i1 < intSizeToInt i2
    "fptrunc"
      | PrimNumFloat size1 <- domType,
        PrimNumFloat size2 <- codType ->
        floatSizeToInt size1 > floatSizeToInt size2
    "fpext"
      | PrimNumFloat size1 <- domType,
        PrimNumFloat size2 <- codType ->
        floatSizeToInt size1 < floatSizeToInt size2
    "fptoui"
      | PrimNumFloat _ <- domType,
        PrimNumInt _ <- codType ->
        True
    "fptosi"
      | PrimNumFloat _ <- domType,
        PrimNumInt _ <- codType ->
        True
    "uitofp"
      | PrimNumInt _ <- domType,
        PrimNumFloat _ <- codType ->
        True
    "sitofp"
      | PrimNumInt _ <- domType,
        PrimNumFloat _ <- codType ->
        True
    _ ->
      False

asLowBinaryOpMaybe' :: T.Text -> PrimNum -> Bool
asLowBinaryOpMaybe' name primNum =
  case primNum of
    PrimNumInt _ ->
      S.member name intBinaryOpSet
    PrimNumFloat _ ->
      S.member name floatBinaryOpSet

asLowICmpMaybe :: T.Text -> Bool
asLowICmpMaybe name =
  S.member name intCmpOpSet

asLowFCmpMaybe :: T.Text -> Bool
asLowFCmpMaybe name =
  S.member name floatCmpOpSet
