module Entity.RawBinder where

import Entity.Hint
import Entity.RawIdent

type RawBinder a =
  (Hint, RawIdent, a)
