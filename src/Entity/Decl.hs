module Entity.Decl where

import Entity.ArgNum qualified as AN
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.Ident
import Entity.IsConstLike

data Decl a = Decl
  { loc :: Hint,
    name :: DD.DefiniteDescription,
    isConstLike :: IsConstLike,
    impArgNum :: AN.ArgNum,
    dom :: [(Hint, Ident, a)],
    cod :: a
  }
