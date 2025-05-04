module Language.RawTerm.Rule.RawBinder (RawBinder) where

import Language.RawTerm.Rule.C
import Language.Common.Rule.Hint
import Language.RawTerm.Rule.RawIdent

type RawBinder a =
  (Hint, RawIdent, C, C, a)
