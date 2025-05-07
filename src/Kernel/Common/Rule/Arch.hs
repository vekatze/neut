module Kernel.Common.Rule.Arch
  ( Arch (..),
    reify,
    dataSizeOf,
  )
where

import Data.Binary
import Data.Text qualified as T
import GHC.Generics qualified as G
import Kernel.Common.Rule.DataSize qualified as DS

-- names are chosen as in https://wiki.debian.org/SupportedArchitectures
data Arch
  = Amd64
  | Arm64
  deriving (Eq, Ord, G.Generic)

instance Binary Arch

reify :: Arch -> T.Text
reify arch =
  case arch of
    Amd64 ->
      "amd64"
    Arm64 ->
      "arm64"

dataSizeOf :: Arch -> DS.DataSize
dataSizeOf arch =
  case arch of
    Amd64 ->
      DS.DataSize64
    Arm64 ->
      DS.DataSize64
