module Entity.RawDecl where

import Entity.ArgNum qualified as AN
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.IsConstLike
import Entity.RawIdent
import Entity.RawTerm qualified as RT

data RawDecl = RawDecl
  { loc :: Hint,
    name :: DD.DefiniteDescription,
    isConstLike :: IsConstLike,
    impArgNum :: AN.ArgNum,
    dom :: [(Hint, RawIdent, RT.RawTerm)],
    cod :: RT.RawTerm
  }
