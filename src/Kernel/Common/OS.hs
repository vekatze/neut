module Kernel.Common.OS
  ( OS (..),
    reify,
  )
where

import Data.Text qualified as T

data OS
  = Linux
  | Darwin
  | Wasip1
  deriving (Eq, Enum, Bounded)

reify :: OS -> T.Text
reify os =
  case os of
    Linux ->
      "linux"
    Darwin ->
      "darwin"
    Wasip1 ->
      "wasip1"
