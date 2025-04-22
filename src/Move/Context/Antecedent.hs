module Move.Context.Antecedent
  ( initialize,
    getMap,
    RevMap,
    getReverseMap,
  )
where

import Data.HashMap.Strict qualified as Map
import Data.Set qualified as S
import Move.Context.App
import Move.Context.App.Internal
import Rule.Module qualified as M
import Rule.ModuleID qualified as MID
import Prelude hiding (lookup, read)

initialize :: App ()
initialize = do
  writeRef' antecedentMap Map.empty
  writeRef' reverseAntecedentMap Map.empty
  writeRef' antecedentDigestCache Nothing

getMap :: App (Map.HashMap MID.ModuleID M.Module)
getMap =
  readRef' antecedentMap

type RevMap =
  Map.HashMap MID.ModuleID (S.Set MID.ModuleID)

getReverseMap :: App RevMap
getReverseMap =
  readRef' reverseAntecedentMap
