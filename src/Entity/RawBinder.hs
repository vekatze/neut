module Entity.RawBinder where

import Entity.C
import Entity.Hint
import Entity.RawIdent

type RawBinder a =
  (Hint, RawIdent, C, C, a)
