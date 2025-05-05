module Language.RawTerm.Rule.RawBinder (RawBinder) where

import Language.Common.Rule.Hint
import Language.RawTerm.Rule.RawIdent
import Tree.Rule.C

type RawBinder a =
  (Hint, RawIdent, C, C, a)
