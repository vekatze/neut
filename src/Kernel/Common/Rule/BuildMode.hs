module Kernel.Common.Rule.BuildMode
  ( BuildMode (..),
    reify,
    fromString,
  )
where

import Data.Text qualified as T
import Error.Rule.Error (Error)
import Language.Common.Rule.Error (newError')

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
