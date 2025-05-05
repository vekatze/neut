module Language.Common.Rule.Geist (Geist (..)) where

import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Ident
import Language.Common.Rule.IsConstLike
import Logger.Rule.Hint

data Geist a = Geist
  { loc :: Hint,
    name :: DD.DefiniteDescription,
    isConstLike :: IsConstLike,
    impArgs :: [(Hint, Ident, a)],
    expArgs :: [(Hint, Ident, a)],
    cod :: a
  }
