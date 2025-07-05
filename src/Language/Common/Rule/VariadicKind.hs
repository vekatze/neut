module Language.Common.Rule.VariadicKind
  ( VariadicKind (..),
    variadicKindToKeyword,
  )
where

import Data.Binary
import Data.Text qualified as T
import GHC.Generics

data VariadicKind
  = VariadicLeft
  | VariadicRight
  deriving (Show, Eq, Generic)

instance Binary VariadicKind

variadicKindToKeyword :: VariadicKind -> T.Text
variadicKindToKeyword vk =
  case vk of
    VariadicLeft ->
      "rule-left"
    VariadicRight ->
      "rule-right"
