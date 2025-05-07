module Kernel.Parse.Move.Internal.Util
  ( readIntBinaryMaybe,
    readIntOctalMaybe,
    readIntDecimalMaybe,
    readIntHexadecimalMaybe,
  )
where

import Data.Text qualified as T

readIntBinaryMaybe :: T.Text -> Maybe Integer
readIntBinaryMaybe = do
  readIntMaybe "0b" binaryMap

readIntOctalMaybe :: T.Text -> Maybe Integer
readIntOctalMaybe = do
  readIntMaybe "0o" octalMap

readIntDecimalMaybe :: T.Text -> Maybe Integer
readIntDecimalMaybe = do
  readIntMaybe "" decimalMap

readIntHexadecimalMaybe :: T.Text -> Maybe Integer
readIntHexadecimalMaybe = do
  readIntMaybe "0x" hexadecimalMap

readIntMaybe :: T.Text -> [(Char, Integer)] -> T.Text -> Maybe Integer
readIntMaybe prefix charMap t = do
  (suffix, sign) <- readSign t
  suffix' <- T.stripPrefix prefix suffix
  v <- interpretAsInteger charMap 1 0 suffix'
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

binaryMap :: [(Char, Integer)]
binaryMap =
  [ ('0', 0),
    ('1', 1)
  ]

octalMap :: [(Char, Integer)]
octalMap =
  [ ('0', 0),
    ('1', 1),
    ('2', 2),
    ('3', 3),
    ('4', 4),
    ('5', 5),
    ('6', 6),
    ('7', 7)
  ]

decimalMap :: [(Char, Integer)]
decimalMap =
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

hexadecimalMap :: [(Char, Integer)]
hexadecimalMap =
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
    ('a', 10),
    ('b', 11),
    ('c', 12),
    ('d', 13),
    ('e', 14),
    ('f', 15)
  ]

interpretAsInteger :: [(Char, Integer)] -> Integer -> Integer -> T.Text -> Maybe Integer
interpretAsInteger charMap base acc t = do
  interpretAsInteger' charMap (fromIntegral (length charMap)) base acc t

interpretAsInteger' :: [(Char, Integer)] -> Integer -> Integer -> Integer -> T.Text -> Maybe Integer
interpretAsInteger' charMap charMapLen base acc t = do
  case T.unsnoc t of
    Nothing ->
      Just acc
    Just (t', '_') -> do
      interpretAsInteger' charMap charMapLen base acc t'
    Just (t', b) -> do
      v <- lookup b charMap
      interpretAsInteger' charMap charMapLen (charMapLen * base) (base * v + acc) t'
