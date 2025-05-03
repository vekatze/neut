module Main.Rule.Binder (BinderF) where

import Main.Rule.Hint
import Main.Rule.Ident

type BinderF a =
  (Hint, Ident, a)
