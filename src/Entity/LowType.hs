{-# LANGUAGE DeriveGeneric #-}

module Entity.LowType where

import Data.Binary (Binary)
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Generics (Generic)
import Text.Read (readMaybe)

data LowType
  = LowTypePrimNum PrimNum
  | LowTypePointer LowType
  | LowTypeArray Int LowType -- [n x LOWTYPE]
  | LowTypeStruct [LowType]
  | LowTypeFunction [LowType] LowType
  deriving (Eq, Ord, Show, Generic)

instance Binary LowType

data PrimNum
  = PrimNumInt IntSize
  | PrimNumFloat FloatSize
  deriving (Show, Generic, Eq, Ord)

instance Binary PrimNum

showPrimNum :: PrimNum -> T.Text
showPrimNum primNum =
  case primNum of
    PrimNumInt size ->
      showIntSize size
    PrimNumFloat size ->
      showFloatSize size

type IntSize =
  Int

data FloatSize
  = FloatSize16
  | FloatSize32
  | FloatSize64
  deriving (Eq, Ord, Show, Generic)

instance Binary FloatSize

data PrimOp
  = PrimOp T.Text [PrimNum] PrimNum
  deriving (Show)

data Magic a
  = MagicCast a a a
  | MagicStore LowType a a
  | MagicLoad LowType a
  | MagicSyscall Integer [a]
  | MagicExternal T.Text [a]
  deriving (Show, Eq, Generic)

instance (Binary a) => Binary (Magic a)

instance Functor Magic where
  fmap f der =
    case der of
      MagicCast from to value ->
        MagicCast (f from) (f to) (f value)
      MagicStore lt pointer value ->
        MagicStore lt (f pointer) (f value)
      MagicLoad lt pointer ->
        MagicLoad lt (f pointer)
      MagicSyscall syscallNum args ->
        MagicSyscall syscallNum $ fmap f args
      MagicExternal extFunName args ->
        MagicExternal extFunName $ fmap f args

instance Foldable Magic where
  foldMap f der =
    case der of
      MagicCast from to value ->
        f from <> f to <> f value
      MagicStore _ pointer value ->
        f pointer <> f value
      MagicLoad _ pointer ->
        f pointer
      MagicSyscall _ args ->
        foldMap f args
      MagicExternal _ args ->
        foldMap f args

instance Traversable Magic where
  traverse f der =
    case der of
      MagicCast from to value ->
        MagicCast <$> f from <*> f to <*> f value
      MagicStore lt pointer value ->
        MagicStore lt <$> f pointer <*> f value
      MagicLoad lt pointer ->
        MagicLoad lt <$> f pointer
      MagicSyscall syscallNum args ->
        MagicSyscall syscallNum <$> traverse f args
      MagicExternal extFunName args ->
        MagicExternal extFunName <$> traverse f args

getMagicName :: Magic a -> T.Text
getMagicName d =
  case d of
    MagicSyscall {} ->
      "syscall"
    MagicExternal {} ->
      "external"
    MagicLoad {} ->
      "load"
    MagicStore {} ->
      "store"
    MagicCast {} ->
      "nop"

asPrimNumMaybe :: T.Text -> Maybe PrimNum
asPrimNumMaybe name
  | Just intSize <- asLowInt name =
    Just $ PrimNumInt intSize
  | Just floatSize <- asLowFloat name =
    Just $ PrimNumFloat floatSize
  | otherwise =
    Nothing

voidPtr :: LowType
voidPtr =
  LowTypePointer (LowTypePrimNum (PrimNumInt 8))

asLowInt :: T.Text -> Maybe IntSize
asLowInt s =
  case T.uncons s of
    Nothing ->
      Nothing
    Just (c, rest) ->
      case c of
        'i'
          | Just n <- readMaybe $ T.unpack rest,
            1 <= n,
            n <= 64 ->
            Just n
        _ ->
          Nothing

asLowFloat :: T.Text -> Maybe FloatSize
asLowFloat s =
  case T.uncons s of
    Nothing ->
      Nothing
    Just (c, rest) ->
      case c of
        'f'
          | Just n <- readMaybe $ T.unpack rest,
            Just size <- asFloatSize n ->
            Just size
        _ ->
          Nothing

asPrimOp :: T.Text -> Maybe PrimOp
asPrimOp name
  | Just ("fneg", typeStr) <- breakOnMaybe "-" name,
    Just primNum@(PrimNumFloat _) <- asPrimNumMaybe typeStr =
    Just $ PrimOp "fneg" [primNum] primNum
  | Just (convOpStr, rest) <- breakOnMaybe "-" name,
    Just (domTypeStr, codTypeStr) <- breakOnMaybe "-" rest,
    Just domType <- asPrimNumMaybe domTypeStr,
    Just codType <- asPrimNumMaybe codTypeStr,
    isValidConvOp convOpStr domType codType =
    Just $ PrimOp convOpStr [domType] codType
  | Just (opStr, typeStr) <- breakOnMaybe "-" name =
    case asPrimNumMaybe typeStr of
      Just primNum@(PrimNumInt _)
        | asLowICmpMaybe opStr ->
          Just $ PrimOp ("icmp " <> opStr) [primNum, primNum] (PrimNumInt 1)
      Just primNum@(PrimNumFloat _)
        | asLowFCmpMaybe opStr ->
          Just $ PrimOp ("fcmp " <> opStr) [primNum, primNum] (PrimNumInt 1)
      Just primNum
        | asLowBinaryOpMaybe' opStr primNum ->
          Just $ PrimOp opStr [primNum, primNum] primNum
      _ ->
        Nothing
  | otherwise =
    Nothing

unaryOpSet :: S.Set T.Text
unaryOpSet =
  S.fromList ["fneg"]

convOpSet :: S.Set T.Text
convOpSet =
  S.fromList ["trunc", "zext", "sext", "fptrunc", "fpext", "fptoui", "fptosi", "uitofp", "sitofp"]

cmpOpSet :: S.Set T.Text
cmpOpSet = do
  let s1 = S.map ("icmp " <>) intCmpOpSet
  let s2 = S.map ("fcmp " <>) floatCmpOpSet
  S.union s1 s2

binaryOpSet :: S.Set T.Text
binaryOpSet =
  S.union intBinaryOpSet floatBinaryOpSet

intCmpOpSet :: S.Set T.Text
intCmpOpSet =
  S.fromList ["eq", "ne", "ugt", "uge", "ult", "ule", "sgt", "sge", "slt", "sle"]

floatCmpOpSet :: S.Set T.Text
floatCmpOpSet =
  S.fromList ["false", "oeq", "ogt", "oge", "olt", "ole", "one", "ord", "ueq", "ugt", "uge", "ult", "ule", "une", "uno", "true"]

intBinaryOpSet :: S.Set T.Text
intBinaryOpSet =
  S.fromList ["add", "sub", "mul", "udiv", "sdiv", "urem", "srem", "shl", "lshr", "ashr", "and", "or", "xor"]

floatBinaryOpSet :: S.Set T.Text
floatBinaryOpSet =
  S.fromList ["fadd", "fsub", "fmul", "fdiv", "frem"]

isValidConvOp :: T.Text -> PrimNum -> PrimNum -> Bool
isValidConvOp name domType codType =
  case name of
    "trunc"
      | PrimNumInt i1 <- domType,
        PrimNumInt i2 <- codType ->
        i1 > i2
    "zext"
      | PrimNumInt i1 <- domType,
        PrimNumInt i2 <- codType ->
        i1 < i2
    "sext"
      | PrimNumInt i1 <- domType,
        PrimNumInt i2 <- codType ->
        i1 < i2
    "fptrunc"
      | PrimNumFloat size1 <- domType,
        PrimNumFloat size2 <- codType ->
        sizeAsInt size1 > sizeAsInt size2
    "fpext"
      | PrimNumFloat size1 <- domType,
        PrimNumFloat size2 <- codType ->
        sizeAsInt size1 < sizeAsInt size2
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

asFloatSize :: Int -> Maybe FloatSize
asFloatSize size =
  case size of
    16 ->
      Just FloatSize16
    32 ->
      Just FloatSize32
    64 ->
      Just FloatSize64
    _ ->
      Nothing

showIntSize :: IntSize -> T.Text
showIntSize size =
  "i" <> T.pack (show size)

showFloatSize :: FloatSize -> T.Text
showFloatSize size =
  "f" <> T.pack (show $ sizeAsInt size)

sizeAsInt :: FloatSize -> Int
sizeAsInt size =
  case size of
    FloatSize16 ->
      16
    FloatSize32 ->
      32
    FloatSize64 ->
      64
