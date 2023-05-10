module Entity.NameArrow
  ( RawNameArrow (..),
    VariantRelatedArrows (..),
    NameArrow,
    InnerRawNameArrow,
    reify,
  )
where

import Entity.DefiniteDescription qualified as DD
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.Name
import GHC.Generics

type NameArrow =
  (NameArrowDom, NameArrowCod)

type NameArrowDom =
  (Hint, DD.DefiniteDescription)

type NameArrowCod =
  (Hint, GN.GlobalName)

data RawNameArrow
  = Function InnerRawNameArrow
  | Variant
      InnerRawNameArrow -- original name
      VariantRelatedArrows -- arrows for constructors/destructors
  deriving (Generic, Show)

data VariantRelatedArrows
  = Explicit [InnerRawNameArrow]
  | Automatic Hint -- "{..}"
  deriving (Show)

type RawNameArrowCod =
  (Hint, Name)

type InnerRawNameArrow =
  (NameArrowDom, RawNameArrowCod)

reify :: NameArrow -> (DD.DefiniteDescription, GN.GlobalName)
reify ((_, aliasName), (_, origDD)) = do
  (aliasName, origDD)
