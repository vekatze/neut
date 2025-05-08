module Kernel.Common.Rule.OS
  ( OS (..),
    reify,
  )
where

import Data.Text qualified as T

data OS
  = Linux
  | Darwin

reify :: OS -> T.Text
reify os =
  case os of
    Linux ->
      "linux"
    Darwin ->
      "darwin"
