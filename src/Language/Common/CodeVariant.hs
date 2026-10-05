module Language.Common.CodeVariant
  ( CodeVariant (..),
    toKeyword,
    getStageOffset,
  )
where

import Data.Binary
import Data.Text qualified as T
import GHC.Generics

data CodeVariant
  = CodeVariantK
  | CodeVariantC -- C: A -> □A (completeness principle)
  deriving (Show, Eq, Generic)

instance Binary CodeVariant

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
