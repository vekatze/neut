module Language.RawTerm.RawBinder (RawBinder) where

import Language.RawTerm.RawIdent
import Logger.Hint
import SyntaxTree.C

type RawBinder a =
  (Hint, RawIdent, C, C, a)
