module Language.Common.PiElimKind
  ( PiElimKind (..),
    fromPiKind,
    fromNoeticPiKind,
    mapArg,
    traverseArg,
    isNormal,
    isNoetic,
    isDestPassing,
  )
where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import Language.Common.PiKind qualified as PK

data PiElimKind t
  = Normal
  | DestPass t
  | Noetic
  | NoeticDestPass t
  deriving (Eq, Ord, Show, Generic)

instance (Binary t) => Binary (PiElimKind t)

fromPiKind :: PK.PiKind -> t -> PiElimKind t
fromPiKind piKind t =
  case piKind of
    PK.DestPass _ ->
      DestPass t
    _ ->
      Normal

fromNoeticPiKind :: PK.PiKind -> t -> PiElimKind t
fromNoeticPiKind piKind t =
  case piKind of
    PK.DestPass _ ->
      NoeticDestPass t
    _ ->
      Noetic

mapArg :: (a -> b) -> PiElimKind a -> PiElimKind b
mapArg f kind =
  case kind of
    Normal ->
      Normal
    DestPass t ->
      DestPass (f t)
    Noetic ->
      Noetic
    NoeticDestPass t ->
      NoeticDestPass (f t)

traverseArg :: (Applicative f) => (a -> f b) -> PiElimKind a -> f (PiElimKind b)
traverseArg f kind =
  case kind of
    Normal ->
      pure Normal
    DestPass t ->
      DestPass <$> f t
    Noetic ->
      pure Noetic
    NoeticDestPass t ->
      NoeticDestPass <$> f t

isNormal :: PiElimKind t -> Bool
isNormal kind =
  case kind of
    Normal ->
      True
    DestPass _ ->
      False
    Noetic ->
      False
    NoeticDestPass _ ->
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
    NoeticDestPass _ ->
      True

isDestPassing :: PiElimKind t -> Bool
isDestPassing kind =
  case kind of
    Normal ->
      False
    DestPass _ ->
      True
    Noetic ->
      False
    NoeticDestPass _ ->
      True
