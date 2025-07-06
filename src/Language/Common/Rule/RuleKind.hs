module Language.Common.Rule.RuleKind
  ( RuleKind (..),
    ruleKindToKeyword,
  )
where

import Data.Binary
import Data.Text qualified as T
import GHC.Generics

data RuleKind
  = FoldLeft
  | FoldRight
  deriving (Show, Eq, Generic)

instance Binary RuleKind

ruleKindToKeyword :: RuleKind -> T.Text
ruleKindToKeyword vk =
  case vk of
    FoldLeft ->
      "rule-left"
    FoldRight ->
      "rule-right"
