module Language.Common.Geist (Geist (..)) where

import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Ident
import Language.Common.IsConstLike
import Logger.Hint

data Geist t a = Geist
  { loc :: Hint,
    name :: DD.DefiniteDescription,
    isConstLike :: IsConstLike,
    impArgs :: [(Hint, Ident, t)],
    expArgs :: [(Hint, Ident, t)],
    defaultArgs :: [((Hint, Ident, t), a)],
    cod :: t
  }
