module Entity.RawDecl where

import Entity.C
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.IsConstLike
import Entity.RawBinder
import Entity.RawTerm qualified as RT

type RawArgs =
  ([(C, RawBinder (RT.RawTerm, C))], C)

type ImpArgs =
  RawArgs

type ExpArgs =
  (Maybe (C, C), RawArgs)

data RawDecl = RawDecl
  { loc :: Hint,
    name :: (DD.DefiniteDescription, C),
    isConstLike :: IsConstLike,
    impArgs :: ImpArgs,
    expArgs :: ExpArgs,
    cod :: (C, (RT.RawTerm, C))
  }
