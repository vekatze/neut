module Kernel.Common.Arch
  ( Arch (..),
    reify,
    dataSizeOf,
    entrySymbol,
  )
where

import Binary.Derive (deriveBinary)
import Data.Text qualified as T
import Language.Common.DataSize qualified as DS

-- names are chosen as in https://wiki.debian.org/SupportedArchitectures
data Arch
  = Amd64
  | Arm64
  | Wasm32
  deriving (Eq, Ord, Enum, Bounded)

reify :: Arch -> T.Text
reify arch =
  case arch of
    Amd64 ->
      "amd64"
    Arm64 ->
      "arm64"
    Wasm32 ->
      "wasm32"

dataSizeOf :: Arch -> DS.DataSize
dataSizeOf arch =
  case arch of
    Amd64 ->
      DS.DataSize64
    Arm64 ->
      DS.DataSize64
    Wasm32 ->
      DS.DataSize32

entrySymbol :: Arch -> T.Text
entrySymbol arch =
  case arch of
    Wasm32 ->
      "__main_argc_argv"
    _ ->
      "main"

deriveBinary [''Arch]
