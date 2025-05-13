module Language.Common.Rule.Binder (BinderF) where

import Logger.Rule.Hint
import Language.Common.Rule.Ident

type BinderF a =
  (Hint, Ident, a)
