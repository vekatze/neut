module Language.Common.Binder (BinderF, getBinderIdent) where

import Language.Common.Ident
import Language.Common.VarKind
import Logger.Hint

type BinderF a =
  (Hint, VarKind, Ident, a)

getBinderIdent :: BinderF a -> Ident
getBinderIdent (_, _, x, _) =
  x
