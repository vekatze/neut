module Language.RawTerm.Rule.RawBinder (RawBinder) where

import Language.RawTerm.Rule.RawIdent
import Library.Logger.Rule.Hint
import Library.SyntaxTree.Rule.C

type RawBinder a =
  (Hint, RawIdent, C, C, a)
