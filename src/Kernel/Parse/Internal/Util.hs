module Kernel.Parse.Internal.Util
  ( readIntBinaryMaybe,
    readIntOctalMaybe,
    readIntDecimalMaybe,
    readIntHexadecimalMaybe,
  )
where

import Data.Text qualified as T

data NumeralSystem = NumeralSystem
  { prefix :: T.Text,
    base :: Integer,
    digitTable :: [(Char, Integer)]
  }

readIntBinaryMaybe :: T.Text -> Maybe Integer
readIntBinaryMaybe = do
  readIntMaybe binarySystem

readIntOctalMaybe :: T.Text -> Maybe Integer
readIntOctalMaybe = do
  readIntMaybe octalSystem

readIntDecimalMaybe :: T.Text -> Maybe Integer
readIntDecimalMaybe = do
  readIntMaybe decimalSystem

readIntHexadecimalMaybe :: T.Text -> Maybe Integer
readIntHexadecimalMaybe = do
  readIntMaybe hexadecimalSystem

readIntMaybe :: NumeralSystem -> T.Text -> Maybe Integer
readIntMaybe numeralSystem t = do
  (suffix, sign) <- readSign t
  suffix' <- T.stripPrefix (prefix numeralSystem) suffix
  if T.length suffix' <= 0
    then Nothing
    else do
      v <- interpretAsInteger numeralSystem 1 0 suffix'
      return $ sign * v

readSign :: T.Text -> Maybe (T.Text, Integer)
readSign t = do
  (c, suffix) <- T.uncons t
  case c of
    '+' ->
      return (suffix, 1)
    '-' ->
      return (suffix, -1)
    _ ->
      return (t, 1)

binarySystem :: NumeralSystem
binarySystem =
  NumeralSystem
    { prefix = "0b",
      base = 2,
      digitTable =
        [ ('0', 0),
          ('1', 1)
        ]
    }

octalSystem :: NumeralSystem
octalSystem =
  NumeralSystem
    { prefix = "0o",
      base = 8,
      digitTable =
        [ ('0', 0),
          ('1', 1),
          ('2', 2),
          ('3', 3),
          ('4', 4),
          ('5', 5),
          ('6', 6),
          ('7', 7)
        ]
    }

decimalSystem :: NumeralSystem
decimalSystem =
  NumeralSystem
    { prefix = "",
      base = 10,
      digitTable =
        [ ('0', 0),
          ('1', 1),
          ('2', 2),
          ('3', 3),
          ('4', 4),
          ('5', 5),
          ('6', 6),
          ('7', 7),
          ('8', 8),
          ('9', 9)
        ]
    }

hexadecimalSystem :: NumeralSystem
hexadecimalSystem =
  NumeralSystem
    { prefix = "0x",
      base = 16,
      digitTable =
        [ ('0', 0),
          ('1', 1),
          ('2', 2),
          ('3', 3),
          ('4', 4),
          ('5', 5),
          ('6', 6),
          ('7', 7),
          ('8', 8),
          ('9', 9),
          ('A', 10),
          ('B', 11),
          ('C', 12),
          ('D', 13),
          ('E', 14),
          ('F', 15)
        ]
    }

interpretAsInteger :: NumeralSystem -> Integer -> Integer -> T.Text -> Maybe Integer
interpretAsInteger numeralSystem currentBase acc t = do
  interpretAsInteger' numeralSystem currentBase acc t

interpretAsInteger' :: NumeralSystem -> Integer -> Integer -> T.Text -> Maybe Integer
interpretAsInteger' numeralSystem currentBase acc t = do
  case T.unsnoc t of
    Nothing ->
      Just acc
    Just (t', '_') -> do
      interpretAsInteger' numeralSystem currentBase acc t'
    Just (t', b) -> do
      v <- lookup b (digitTable numeralSystem)
      interpretAsInteger' numeralSystem (base numeralSystem * currentBase) (currentBase * v + acc) t'
