module Entity.Decl where

import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.Ident
import Entity.IsConstLike

data Decl a = Decl
  { loc :: Hint,
    name :: DD.DefiniteDescription,
    isConstLike :: IsConstLike,
    impArgs :: [(Hint, Ident, a)],
    expArgs :: [(Hint, Ident, a)],
    cod :: a
  }
