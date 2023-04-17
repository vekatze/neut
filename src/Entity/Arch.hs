module Entity.Arch
  ( Arch (..),
    reify,
    reflect,
    addressBitSize,
  )
where

import Data.Text qualified as T

-- names are chosen as in https://wiki.debian.org/SupportedArchitectures
data Arch
  = Amd64
  | Arm64
  | Unknown T.Text

reify :: Arch -> T.Text
reify arch =
  case arch of
    Amd64 ->
      "amd64"
    Arm64 ->
      "arm64"
    Unknown name ->
      name

reflect :: T.Text -> Arch
reflect name =
  case name of
    "amd64" ->
      Amd64
    "x86_64" ->
      Amd64
    "arm64" ->
      Arm64
    "aarch64" ->
      Arm64
    _ ->
      Unknown name

addressBitSize :: Arch -> Maybe Int
addressBitSize arch =
  case arch of
    Amd64 ->
      Just 64
    Arm64 ->
      Just 64
    Unknown _ ->
      Nothing
