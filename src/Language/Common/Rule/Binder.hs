module Language.Common.Rule.Binder (BinderF) where

import Language.Common.Rule.Ident
import Library.Logger.Rule.Hint

type BinderF a =
  (Hint, Ident, a)
