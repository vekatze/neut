module Language.RawTerm.RawBinder (RawBinder) where

import Language.Common.VarKind
import Language.RawTerm.RawIdent
import Logger.Hint
import SyntaxTree.C

type RawBinder a =
  (Hint, VarKind, RawIdent, C, C, a)
