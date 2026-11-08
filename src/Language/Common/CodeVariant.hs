module Language.Common.CodeVariant
  ( CodeVariant (..),
    toKeyword,
    getStageOffset,
  )
where

import Binary.Derive (deriveBinary)
import Data.Text qualified as T

data CodeVariant
  = CodeVariantK
  | CodeVariantC -- C: A -> □A (completeness principle)
  deriving (Show, Eq)

toKeyword :: CodeVariant -> T.Text
toKeyword v =
  case v of
    CodeVariantK ->
      "quote"
    CodeVariantC ->
      "promote"

getStageOffset :: CodeVariant -> Int
getStageOffset v =
  case v of
    CodeVariantK ->
      1
    CodeVariantC ->
      0

deriveBinary [''CodeVariant]
