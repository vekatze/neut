module Rule.Binder (BinderF) where

import Rule.Hint
import Rule.Ident

type BinderF a =
  (Hint, Ident, a)
