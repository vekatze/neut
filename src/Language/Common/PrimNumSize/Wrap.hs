module Language.Common.PrimNumSize.Wrap
  ( unsignedOf,
    signedOf,
  )
where

import Data.Bits (shiftL)
import Language.Common.PrimNumSize
import Language.Common.PrimNumSize.ToInt (intSizeToInt)

unsignedOf :: IntSize -> Integer -> Integer
unsignedOf size value =
  value `mod` (1 `shiftL` intSizeToInt size)

signedOf :: IntSize -> Integer -> Integer
signedOf size value = do
  let modulus = 1 `shiftL` intSizeToInt size
  let unsigned = value `mod` modulus
  if unsigned < modulus `div` 2
    then unsigned
    else unsigned - modulus
