module Entity.Binder (BinderF) where

import Entity.Hint
import Entity.Ident

type BinderF a =
  (Hint, Ident, a)
