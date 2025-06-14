module Language.Common.Rule.Geist (Geist (..)) where

import Logger.Rule.Hint
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Ident
import Language.Common.Rule.IsConstLike

data Geist a = Geist
  { loc :: Hint,
    name :: DD.DefiniteDescription,
    isConstLike :: IsConstLike,
    impArgs :: [((Hint, Ident, a), Maybe a)],
    expArgs :: [(Hint, Ident, a)],
    cod :: a
  }
