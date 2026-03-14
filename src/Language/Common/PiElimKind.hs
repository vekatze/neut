module Language.Common.PiElimKind
  ( PiElimKind (..),
    fromPiKind,
    isNoetic,
  )
where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import Language.Common.PiKind qualified as PK

data PiElimKind
  = Normal
  | DestPass
  | Noetic
  deriving (Eq, Ord, Show, Generic)

instance Binary PiElimKind

fromPiKind :: PK.PiKind -> PiElimKind
fromPiKind piKind =
  case piKind of
    PK.DestPass _ ->
      DestPass
    _ ->
      Normal

isNoetic :: PiElimKind -> Bool
isNoetic kind =
  case kind of
    Normal ->
      False
    DestPass ->
      False
    Noetic ->
      True
