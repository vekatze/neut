module Entity.RawDecl where

import Entity.C
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.IsConstLike
import Entity.RawTerm qualified as RT

data RawDecl = RawDecl
  { loc :: Hint,
    name :: (DD.DefiniteDescription, C),
    isConstLike :: IsConstLike,
    impArgs :: RT.Args RT.RawTerm,
    expArgs :: RT.Args RT.RawTerm,
    cod :: (C, (RT.RawTerm, C))
  }
