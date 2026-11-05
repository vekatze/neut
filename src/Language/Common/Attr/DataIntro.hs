module Language.Common.Attr.DataIntro (Attr (..)) where

import Binary.Derive (deriveBinary)
import Language.Common.Discriminant qualified as D
import Language.Common.DefiniteDescription qualified as DD
import Language.Common.IsConstLike

data Attr = Attr
  { dataName :: DD.DefiniteDescription,
    discriminant :: D.Discriminant,
    isConstLike :: IsConstLike
  }
  deriving (Show, Eq)

deriveBinary [''Attr]
