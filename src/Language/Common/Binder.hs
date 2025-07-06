module Language.Common.Binder (BinderF) where

import Language.Common.Ident
import Logger.Hint

type BinderF a =
  (Hint, Ident, a)
