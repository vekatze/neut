module Language.Common.Binder (BinderF) where

import Language.Common.Ident
import Language.Common.VarKind
import Logger.Hint

type BinderF a =
  (Hint, VarKind, Ident, a)
