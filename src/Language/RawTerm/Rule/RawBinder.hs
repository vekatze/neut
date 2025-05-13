module Language.RawTerm.Rule.RawBinder (RawBinder) where

import Logger.Rule.Hint
import SyntaxTree.Rule.C
import Language.RawTerm.Rule.RawIdent

type RawBinder a =
  (Hint, RawIdent, C, C, a)
