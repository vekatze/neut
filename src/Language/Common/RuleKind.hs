module Language.Common.RuleKind
  ( RuleKind (..),
    ruleKindToKeyword,
  )
where

import Binary.Derive (deriveBinary)
import Data.Text qualified as T

data RuleKind
  = FoldLeft
  | FoldRight
  deriving (Show, Eq)

ruleKindToKeyword :: RuleKind -> T.Text
ruleKindToKeyword vk =
  case vk of
    FoldLeft ->
      "rule-left"
    FoldRight ->
      "rule-right"

deriveBinary [''RuleKind]
