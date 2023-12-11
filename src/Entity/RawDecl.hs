module Entity.RawDecl where

import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.IsConstLike
import Entity.RawBinder
import Entity.RawTerm qualified as RT

data RawDecl = RawDecl
  { loc :: Hint,
    name :: DD.DefiniteDescription,
    isConstLike :: IsConstLike,
    impArgs :: [RawBinder RT.RawTerm],
    expArgs :: [RawBinder RT.RawTerm],
    cod :: RT.RawTerm
  }
