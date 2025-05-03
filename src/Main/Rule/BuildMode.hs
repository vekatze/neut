module Main.Rule.BuildMode
  ( BuildMode (..),
    reify,
  )
where

import Data.Text qualified as T

data BuildMode
  = Develop
  | Release

reify :: BuildMode -> T.Text
reify bm =
  case bm of
    Develop ->
      "develop"
    Release ->
      "release"
