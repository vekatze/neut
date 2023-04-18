module Entity.PrimOp.FromText (fromDefiniteDescription) where

import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.BaseName qualified as BN
import Entity.DataSize qualified as DS
import Entity.DefiniteDescription qualified as DD
import Entity.LocalLocator qualified as LL
import Entity.PrimNumSize.ToInt
import Entity.PrimOp
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
    isValidConvOp convOpStr domType codType =
      Just $ PrimConvOp convOpStr domType codType
  | Just (opStr, typeStr) <- breakOnMaybe "-" name,
    Just primType <- PT.fromText dataSize typeStr =
      getOp1 opStr primType
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

-- turn "<op>-<type>" (e.g. add-u64) into a PrimOp
getOp1 :: T.Text -> PT.PrimType -> Maybe PrimOp
getOp1 rawOpName primType =
  case primType of
    PT.Int {}
      | Just primOpGen <- Map.lookup rawOpName signedIntOpDict ->
          return $ primOpGen primType
    PT.UInt {}
      | Just primOpGen <- Map.lookup rawOpName unsignedIntOpDict ->
          return $ primOpGen primType
    PT.Float {}
      | Just primOpGen <- Map.lookup rawOpName floatOpDict ->
          return $ primOpGen primType
    _ ->
      Nothing

signedIntOpDict :: Map.HashMap T.Text (PT.PrimType -> PrimOp)
signedIntOpDict =
  Map.union baseIntOpDict $
    Map.fromList
      [ ("div", binOp "sdiv"),
        ("rem", binOp "srem"),
        ("gt", cmpOp "sgt"),
        ("ge", cmpOp "sge"),
        ("lt", cmpOp "slt"),
        ("le", cmpOp "sle")
      ]

unsignedIntOpDict :: Map.HashMap T.Text (PT.PrimType -> PrimOp)
unsignedIntOpDict =
  Map.union baseIntOpDict $
    Map.fromList
      [ ("div", binOp "udiv"),
        ("rem", binOp "urem"),
        ("gt", cmpOp "ugt"),
        ("ge", cmpOp "uge"),
        ("lt", cmpOp "ult"),
        ("le", cmpOp "ule")
      ]

baseIntOpDict :: Map.HashMap T.Text (PT.PrimType -> PrimOp)
baseIntOpDict =
  Map.fromList
    [ ("add", binOp "add"),
      ("sub", binOp "sub"),
      ("mul", binOp "mul"),
      ("and", binOp "and"),
      ("or", binOp "or"),
      ("xor", binOp "xor"),
      ("shl", binOp "shl"),
      ("lshr", binOp "lshr"),
      ("ashr", binOp "ashr"),
      ("eq", cmpOp "eq"),
      ("ne", cmpOp "ne")
    ]

floatOpDict :: Map.HashMap T.Text (PT.PrimType -> PrimOp)
floatOpDict =
  Map.fromList
    [ ("neg", unaryOp "fneg"),
      ("add", binOp "fadd"),
      ("sub", binOp "fsub"),
      ("mul", binOp "fmul"),
      ("div", binOp "fdiv"),
      ("rem", binOp "frem"),
      ("oeq", cmpOp "oeq"),
      ("one", cmpOp "one"),
      ("ogt", cmpOp "ogt"),
      ("oge", cmpOp "oge"),
      ("olt", cmpOp "olt"),
      ("ole", cmpOp "ole"),
      ("ueq", cmpOp "ueq"),
      ("ugt", cmpOp "ugt"),
      ("uge", cmpOp "uge"),
      ("ult", cmpOp "ult"),
      ("ule", cmpOp "ule"),
      ("une", cmpOp "une"),
      ("ord", cmpOp "ord"),
      ("uno", cmpOp "uno"),
      ("false", cmpOp "false"),
      ("true", cmpOp "true")
    ]
