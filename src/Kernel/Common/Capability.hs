module Kernel.Common.Capability
  ( Capability (..),
    reify,
    reflect,
    everySubset,
    providedBy,
  )
where

import Data.Set qualified as S
import Data.Text qualified as T
import Kernel.Common.OS qualified as OS

data Capability
  = Thread
  | Subprocess
  deriving (Eq, Ord, Show, Enum, Bounded)

reify :: Capability -> T.Text
reify capability =
  case capability of
    Thread ->
      "thread"
    Subprocess ->
      "subprocess"

reflect :: T.Text -> Maybe Capability
reflect text =
  case text of
    "thread" ->
      Just Thread
    "subprocess" ->
      Just Subprocess
    _ ->
      Nothing

everySubset :: [S.Set Capability]
everySubset =
  map S.fromList $ subsequenceList [minBound .. maxBound]

subsequenceList :: [a] -> [[a]]
subsequenceList xs =
  case xs of
    [] ->
      [[]]
    y : rest -> do
      let rest' = subsequenceList rest
      rest' ++ map (y :) rest'

providedBy :: OS.OS -> S.Set Capability
providedBy os =
  case os of
    OS.Linux ->
      S.fromList [Thread, Subprocess]
    OS.Darwin ->
      S.fromList [Thread, Subprocess]
    OS.Wasi ->
      S.empty
