module Kernel.Emit.Builder
  ( unwordsC,
    unwordsL,
    unlinesL,
  )
where

import Data.ByteString.Builder

_intercalate :: Builder -> [Builder] -> Builder
_intercalate sep bs =
  case bs of
    [] ->
      ""
    [b] ->
      b
    b : rest ->
      b <> sep <> _intercalate sep rest

{-# INLINE unwordsC #-}
unwordsC :: [Builder] -> Builder
unwordsC =
  _intercalate ", "

{-# INLINE unwordsL #-}
unwordsL :: [Builder] -> Builder
unwordsL =
  _intercalate " "

{-# INLINE unlinesL #-}
unlinesL :: [Builder] -> Builder
unlinesL =
  _intercalate "\n"
