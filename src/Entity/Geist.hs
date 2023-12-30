module Entity.Geist (Geist (..)) where

import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.Ident
import Entity.IsConstLike

data Geist a = Geist
  { loc :: Hint,
    name :: DD.DefiniteDescription,
    isConstLike :: IsConstLike,
    impArgs :: [(Hint, Ident, a)],
    expArgs :: [(Hint, Ident, a)],
    cod :: a
  }
