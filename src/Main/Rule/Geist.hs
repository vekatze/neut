module Main.Rule.Geist (Geist (..)) where

import Main.Rule.DefiniteDescription qualified as DD
import Main.Rule.Hint
import Main.Rule.Ident
import Main.Rule.IsConstLike

data Geist a = Geist
  { loc :: Hint,
    name :: DD.DefiniteDescription,
    isConstLike :: IsConstLike,
    impArgs :: [(Hint, Ident, a)],
    expArgs :: [(Hint, Ident, a)],
    cod :: a
  }
