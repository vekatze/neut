module Language.Common.Geist (Geist (..)) where

import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Ident
import Language.Common.IsConstLike
import Logger.Hint

data Geist a = Geist
  { loc :: Hint,
    name :: DD.DefiniteDescription,
    isConstLike :: IsConstLike,
    impArgs :: [((Hint, Ident, a), Maybe a)],
    expArgs :: [(Hint, Ident, a)],
    cod :: a
  }
