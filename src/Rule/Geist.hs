module Rule.Geist (Geist (..)) where

import Rule.DefiniteDescription qualified as DD
import Rule.Hint
import Rule.Ident
import Rule.IsConstLike

data Geist a = Geist
  { loc :: Hint,
    name :: DD.DefiniteDescription,
    isConstLike :: IsConstLike,
    impArgs :: [(Hint, Ident, a)],
    expArgs :: [(Hint, Ident, a)],
    cod :: a
  }
