module Kernel.Common.Capability
  ( Capability (..),
    reify,
    reflect,
    everyProvidedSet,
    providedBy,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.Set qualified as S
import Data.Text qualified as T
import Kernel.Common.Platform qualified as P

data Capability
  = Thread
  | Subprocess
  | JavaScript
  deriving (Eq, Ord, Show, Enum, Bounded)

reify :: Capability -> T.Text
reify capability =
  case capability of
    Thread ->
      "thread"
    Subprocess ->
      "subprocess"
    JavaScript ->
      "javascript"

reflect :: T.Text -> Maybe Capability
reflect text =
  case text of
    "thread" ->
      Just Thread
    "subprocess" ->
      Just Subprocess
    "javascript" ->
      Just JavaScript
    _ ->
      Nothing

providedBy :: P.PlatformName -> S.Set Capability
providedBy name =
  case name of
    P.Arm64Darwin ->
      S.fromList [Thread, Subprocess]
    P.Amd64Linux ->
      S.fromList [Thread, Subprocess]
    P.Arm64Linux ->
      S.fromList [Thread, Subprocess]
    P.Wasm32Wasip1 ->
      S.empty
    P.Wasm32Wasip1Web ->
      S.fromList [JavaScript]

everyProvidedSet :: [S.Set Capability]
everyProvidedSet =
  nubOrd $ map providedBy [minBound .. maxBound]
