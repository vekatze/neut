module Entity.NameArrow
  ( RawNameArrow (..),
    NameArrow,
    InnerRawNameArrow,
  )
where

import Entity.DefiniteDescription qualified as DD
import Entity.GlobalName qualified as GN
import Entity.Hint
import Entity.Name
import GHC.Generics

type NameArrow =
  (DD.DefiniteDescription, GN.GlobalName)

data RawNameArrow
  = Function InnerRawNameArrow
  | Variant InnerRawNameArrow
  deriving (Generic, Show)

type InnerRawNameArrow =
  (Hint, Name)
