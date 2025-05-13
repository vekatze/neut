module Kernel.Common.Rule.BuildMode
  ( BuildMode (..),
    reify,
    fromString,
  )
where

import Error.Rule.Error (Error, newError')
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

fromString :: String -> Either Error BuildMode
fromString input = do
  case input of
    "develop" ->
      return Develop
    "release" ->
      return Release
    _ ->
      Left $ newError' $ "No such build mode exists: " <> T.pack input
