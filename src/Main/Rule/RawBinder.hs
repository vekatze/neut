module Main.Rule.RawBinder (RawBinder) where

import Main.Rule.C
import Main.Rule.Hint
import Main.Rule.RawIdent

type RawBinder a =
  (Hint, RawIdent, C, C, a)
