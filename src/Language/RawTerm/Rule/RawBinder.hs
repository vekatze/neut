module Language.RawTerm.Rule.RawBinder (RawBinder) where

import Language.RawTerm.Rule.RawIdent
import Logger.Rule.Hint
import SyntaxTree.Rule.C

type RawBinder a =
  (Hint, RawIdent, C, C, a)
