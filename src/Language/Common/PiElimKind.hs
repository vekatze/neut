module Language.Common.PiElimKind
  ( PiElimKind (..),
    fromPiKind,
    mapArg,
    traverseArg,
    isNormal,
    isNoetic,
  )
where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import Language.Common.PiKind qualified as PK

data PiElimKind t
  = Normal
  | DestPass t
  | Noetic
  deriving (Eq, Ord, Show, Generic)

instance (Binary t) => Binary (PiElimKind t)

fromPiKind :: PK.PiKind -> t -> PiElimKind t
fromPiKind piKind t =
  case piKind of
    PK.DestPass _ ->
      DestPass t
    _ ->
      Normal

mapArg :: (a -> b) -> PiElimKind a -> PiElimKind b
mapArg f kind =
  case kind of
    Normal ->
      Normal
    DestPass t ->
      DestPass (f t)
    Noetic ->
      Noetic

traverseArg :: (Applicative f) => (a -> f b) -> PiElimKind a -> f (PiElimKind b)
traverseArg f kind =
  case kind of
    Normal ->
      pure Normal
    DestPass t ->
      DestPass <$> f t
    Noetic ->
      pure Noetic

isNormal :: PiElimKind t -> Bool
isNormal kind =
  case kind of
    Normal ->
      True
    DestPass _ ->
      False
    Noetic ->
      False

isNoetic :: PiElimKind t -> Bool
isNoetic kind =
  case kind of
    Normal ->
      False
    DestPass _ ->
      False
    Noetic ->
      True
