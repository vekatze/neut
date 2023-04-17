module Entity.OS
  ( OS (..),
    reify,
    reflect,
  )
where

import Data.Text qualified as T

data OS
  = Linux
  | Darwin
  | Unknown T.Text

reify :: OS -> T.Text
reify os =
  case os of
    Linux ->
      "linux"
    Darwin ->
      "darwin"
    Unknown name ->
      name

reflect :: T.Text -> OS
reflect name =
  case name of
    "linux" ->
      Linux
    "darwin" ->
      Darwin
    _ ->
      Unknown name
