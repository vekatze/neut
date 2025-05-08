module Language.RawTerm.Rule.RawBinder (RawBinder) where

import Aux.Logger.Rule.Hint
import Aux.SyntaxTree.Rule.C
import Language.RawTerm.Rule.RawIdent

type RawBinder a =
  (Hint, RawIdent, C, C, a)
