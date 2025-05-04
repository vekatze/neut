module Language.Common.Rule.Binder (BinderF) where

import Language.Common.Rule.Hint
import Language.Common.Rule.Ident

type BinderF a =
  (Hint, Ident, a)
