module Kernel.Parse.Internal.Util
  ( NumericClass (..),
    NumericLiteral (..),
    NumericParseResult (..),
    isNumericLike,
    numericClassToText,
    parseNumericLiteral,
  )
where

import Control.Monad (guard)
import Data.Char (digitToInt, isDigit)
import Data.List (find)
import Data.Maybe (isJust)
import Data.Ratio ((%))
import Data.Text qualified as T

data NumericClass
  = BinaryInt
  | OctalInt
  | DecimalInt
  | HexadecimalInt
  | DecimalFloat
  | HexadecimalFloat
  deriving (Eq, Show)

data NumericLiteral
  = IntegerLiteral Integer
  | FloatingLiteral Double
  deriving (Eq, Show)

data NumericParseResult
  = NotNumeric
  | InvalidNumericLiteral NumericClass
  | ParsedNumericLiteral NumericLiteral
  deriving (Eq, Show)

data NumeralSystem = NumeralSystem
  { prefix :: T.Text,
    base :: Integer,
    digitTable :: [(Char, Integer)]
  }

isNumericLike :: T.Text -> Bool
isNumericLike text =
  parseNumericLiteral text /= NotNumeric

numericClassToText :: NumericClass -> T.Text
numericClassToText numericClass =
  case numericClass of
    BinaryInt ->
      "binary integer"
    OctalInt ->
      "octal integer"
    DecimalInt ->
      "decimal integer"
    HexadecimalInt ->
      "hexadecimal integer"
    DecimalFloat ->
      "floating-point"
    HexadecimalFloat ->
      "hexadecimal floating-point"

parseNumericLiteral :: T.Text -> NumericParseResult
parseNumericLiteral text
  | isHexadecimalFloatCandidate unsignedText =
      parseFloatingLiteral HexadecimalFloat (readFloatingMaybe hexadecimalSystem 'p' 2) normalizedText
  | Just (numericClass, numeralSystem) <- find (\(_, system) -> hasPrefix system unsignedText) prefixedIntegerSystems =
      parseIntegerLiteral numericClass numeralSystem normalizedText
  | isDecimalIntegerCandidate unsignedText =
      parseIntegerLiteral DecimalInt decimalSystem normalizedText
  | isDecimalFloatCandidate unsignedText =
      parseFloatingLiteral DecimalFloat (readFloatingMaybe decimalSystem 'e' 10) normalizedText
  | otherwise =
      NotNumeric
  where
    normalizedText = dropUnderscores text
    unsignedText = dropOptionalSign normalizedText

parseIntegerLiteral :: NumericClass -> NumeralSystem -> T.Text -> NumericParseResult
parseIntegerLiteral numericClass numeralSystem text =
  case readIntMaybe numeralSystem text of
    Just value ->
      ParsedNumericLiteral $ IntegerLiteral value
    Nothing ->
      InvalidNumericLiteral numericClass

parseFloatingLiteral :: NumericClass -> (T.Text -> Maybe Double) -> T.Text -> NumericParseResult
parseFloatingLiteral numericClass readValue text =
  case readValue text of
    Just value ->
      ParsedNumericLiteral $ FloatingLiteral value
    Nothing ->
      InvalidNumericLiteral numericClass

hasPrefix :: NumeralSystem -> T.Text -> Bool
hasPrefix numeralSystem =
  T.isPrefixOf (prefix numeralSystem)

dropOptionalSign :: T.Text -> T.Text
dropOptionalSign =
  fst . readIntegerSign

readIntMaybe :: NumeralSystem -> T.Text -> Maybe Integer
readIntMaybe numeralSystem text = do
  (suffix, sign) <- readSignedSuffix numeralSystem text
  (digits, rest) <- readRequiredDigitValues numeralSystem suffix
  guard $ T.null rest
  return $ sign * digitsToInteger (base numeralSystem) digits

readFloatingMaybe :: NumeralSystem -> Char -> Integer -> T.Text -> Maybe Double
readFloatingMaybe numeralSystem exponentMarker exponentBase text = do
  (suffix, sign) <- readSignedSuffix numeralSystem text
  (wholeDigits, rest) <- readRequiredDigitValues numeralSystem suffix
  let wholeValue = digitsToRational (base numeralSystem) wholeDigits
  (fractionValue, rest') <- readFractionValue numeralSystem rest
  (exponentSign, exponentValue, exponentRest) <- parseExponentWithMarker exponentMarker rest'
  guard $ T.null exponentRest
  let mantissaValue = wholeValue + fractionValue
  let exponentScale = exponentToScale exponentBase exponentSign exponentValue
  let finalValue = mantissaValue * exponentScale
  let doubleValue = fromIntegral sign * fromRational finalValue
  guard $ isFiniteDouble doubleValue
  return doubleValue

readIntegerSign :: T.Text -> (T.Text, Integer)
readIntegerSign text =
  case T.uncons text of
    Just ('+', rest) ->
      (rest, 1)
    Just ('-', rest) ->
      (rest, -1)
    _ ->
      (text, 1)

dropUnderscores :: T.Text -> T.Text
dropUnderscores =
  T.filter (/= '_')

readSignedSuffix :: NumeralSystem -> T.Text -> Maybe (T.Text, Integer)
readSignedSuffix numeralSystem text = do
  let normalizedText = dropUnderscores text
  let (suffix, sign) = readIntegerSign normalizedText
  suffix' <- T.stripPrefix (prefix numeralSystem) suffix
  return (suffix', sign)

isDecimalIntegerCandidate :: T.Text -> Bool
isDecimalIntegerCandidate text =
  startsWith isDigit text && T.all isDigit text

isDecimalFloatCandidate :: T.Text -> Bool
isDecimalFloatCandidate text =
  startsWith isDigit text && T.any isFloatMarker text

isHexadecimalFloatCandidate :: T.Text -> Bool
isHexadecimalFloatCandidate text =
  case T.stripPrefix (prefix hexadecimalSystem) text of
    Just suffix ->
      T.any isHexadecimalFloatMarker suffix
    Nothing ->
      False

startsWith :: (Char -> Bool) -> T.Text -> Bool
startsWith predicate text =
  case T.uncons text of
    Just (c, _) ->
      predicate c
    Nothing ->
      False

isFloatMarker :: Char -> Bool
isFloatMarker c =
  c == '.' || c == 'e'

isHexadecimalFloatMarker :: Char -> Bool
isHexadecimalFloatMarker c =
  c == '.' || c == 'p'

parseExponentWithMarker :: Char -> T.Text -> Maybe (Integer, Integer, T.Text)
parseExponentWithMarker marker text =
  case T.uncons text of
    Just (c, rest)
      | c == marker -> do
          guard $ not $ T.null rest
          let (rest', sign) = readIntegerSign rest
          let (digits, exponentRest) = readDigits isDigit rest'
          guard $ not $ T.null digits
          let value = digitsToInteger 10 $ map digitToInteger $ T.unpack digits
          return (sign, value, exponentRest)
    _ ->
      return (1, 0, text)

readRequiredDigitValues :: NumeralSystem -> T.Text -> Maybe ([Integer], T.Text)
readRequiredDigitValues numeralSystem text = do
  let (digitChars, rest) = readDigitsOf numeralSystem text
  guard $ not $ T.null digitChars
  digits <- mapM (digitValue numeralSystem) $ T.unpack digitChars
  return (digits, rest)

readFractionValue :: NumeralSystem -> T.Text -> Maybe (Rational, T.Text)
readFractionValue numeralSystem text =
  case T.uncons text of
    Just ('.', fractionText) -> do
      (fractionDigits, fractionRest) <- readRequiredDigitValues numeralSystem fractionText
      let fractionValue = digitsToFractionRational (base numeralSystem) fractionDigits
      return (fractionValue, fractionRest)
    _ ->
      Nothing

readDigits :: (Char -> Bool) -> T.Text -> (T.Text, T.Text)
readDigits =
  T.span

readDigitsOf :: NumeralSystem -> T.Text -> (T.Text, T.Text)
readDigitsOf numeralSystem =
  readDigits (isDigitOf numeralSystem)

isDigitOf :: NumeralSystem -> Char -> Bool
isDigitOf numeralSystem =
  hasDigitValue $ digitValue numeralSystem

hasDigitValue :: (Char -> Maybe Integer) -> Char -> Bool
hasDigitValue digitReader =
  isJust . digitReader

digitValue :: NumeralSystem -> Char -> Maybe Integer
digitValue numeralSystem c =
  lookup c (digitTable numeralSystem)

digitToInteger :: Char -> Integer
digitToInteger c =
  fromIntegral $ digitToInt c

isFiniteDouble :: Double -> Bool
isFiniteDouble value =
  not (isInfinite value || isNaN value)

digitsToRational :: Integer -> [Integer] -> Rational
digitsToRational baseValue digits =
  digitsToInteger baseValue digits % 1

digitsToInteger :: Integer -> [Integer] -> Integer
digitsToInteger baseValue =
  foldl (\acc digit -> acc * baseValue + digit) 0

digitsToFractionRational :: Integer -> [Integer] -> Rational
digitsToFractionRational baseValue digits = do
  let numerator = digitsToInteger baseValue digits
  let denominator = baseValue ^ length digits
  numerator % denominator

exponentToScale :: Integer -> Integer -> Integer -> Rational
exponentToScale baseValue exponentSign exponentValue
  | exponentSign < 0 =
      1 % (baseValue ^ exponentValue)
  | otherwise =
      (baseValue ^ exponentValue) % 1

mkNumeralSystem :: T.Text -> Integer -> String -> NumeralSystem
mkNumeralSystem numeralPrefix numeralBase digits =
  NumeralSystem
    { prefix = numeralPrefix,
      base = numeralBase,
      digitTable = zip digits [0 ..]
    }

prefixedIntegerSystems :: [(NumericClass, NumeralSystem)]
prefixedIntegerSystems =
  [ (BinaryInt, binarySystem),
    (OctalInt, octalSystem),
    (HexadecimalInt, hexadecimalSystem)
  ]

binarySystem :: NumeralSystem
binarySystem =
  mkNumeralSystem "0b" 2 "01"

octalSystem :: NumeralSystem
octalSystem =
  mkNumeralSystem "0o" 8 "01234567"

decimalSystem :: NumeralSystem
decimalSystem =
  mkNumeralSystem "" 10 "0123456789"

hexadecimalSystem :: NumeralSystem
hexadecimalSystem =
  mkNumeralSystem "0x" 16 "0123456789ABCDEF"
