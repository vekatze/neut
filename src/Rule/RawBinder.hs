module Rule.RawBinder (RawBinder) where

import Rule.C
import Rule.Hint
import Rule.RawIdent

type RawBinder a =
  (Hint, RawIdent, C, C, a)
