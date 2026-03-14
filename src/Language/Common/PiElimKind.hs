module Language.Common.PiElimKind
  ( PiElimKind (..),
    isNoetic,
  )
where

import Data.Binary (Binary)
import GHC.Generics (Generic)

data PiElimKind
  = Normal
  | Noetic
  deriving (Eq, Ord, Show, Generic)

instance Binary PiElimKind

isNoetic :: PiElimKind -> Bool
isNoetic kind =
  case kind of
    Normal ->
      False
    Noetic ->
      True
