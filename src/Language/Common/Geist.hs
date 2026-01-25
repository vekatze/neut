module Language.Common.Geist (Geist (..)) where

import Language.Common.Binder
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.Ident
import Language.Common.IsConstLike
import Logger.Hint

data Geist t a = Geist
  { loc :: Hint,
    name :: DD.DefiniteDescription,
    isConstLike :: IsConstLike,
    impArgs :: [BinderF t],
    expArgs :: [BinderF t],
    defaultArgs :: [(BinderF t, a)],
    cod :: t
  }
