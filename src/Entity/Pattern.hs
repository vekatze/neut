module Entity.Pattern where

import Entity.Arity
import Entity.Binder
import qualified Entity.DefiniteDescription as DD
import Entity.Hint
import qualified Entity.UnresolvedName as UN

type PatternF a =
  (Hint, DD.DefiniteDescription, Arity, [BinderF a])

type PrePatternF a =
  (Hint, Either UN.UnresolvedName DD.DefiniteDescription, [BinderF a])
