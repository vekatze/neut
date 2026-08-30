module Kernel.Common.Capability
  ( Capability (..),
    reify,
    reflect,
    everyProvidedSet,
    providedBy,
  )
where

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

providedBy :: P.PlatformSelector -> S.Set Capability
providedBy selector =
  case selector of
    P.SelectHost ->
      S.fromList [Thread, Subprocess]
    P.SelectWasm32 ->
      S.empty
    P.SelectWeb ->
      S.fromList [JavaScript]

everyProvidedSet :: [S.Set Capability]
everyProvidedSet =
  map providedBy [minBound .. maxBound]
