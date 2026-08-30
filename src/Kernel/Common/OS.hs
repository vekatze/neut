module Kernel.Common.OS
  ( OS (..),
    reify,
  )
where

import Data.Text qualified as T

data OS
  = Linux
  | Darwin
  | Wasi

reify :: OS -> T.Text
reify os =
  case os of
    Linux ->
      "linux"
    Darwin ->
      "darwin"
    Wasi ->
      "wasi"
