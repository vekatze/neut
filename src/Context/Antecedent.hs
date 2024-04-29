module Context.Antecedent where

import Context.App
import Context.App.Internal
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Entity.Module qualified as M
import Entity.ModuleDigest qualified as MD
import Entity.ModuleID qualified as MID
import Prelude hiding (lookup, read)

setMap :: Map.HashMap MID.ModuleID M.Module -> App ()
setMap =
  writeRef' antecedentMap

getMap :: App (Map.HashMap MID.ModuleID M.Module)
getMap =
  readRef' antecedentMap

lookup :: MD.ModuleDigest -> App (Maybe M.Module)
lookup mc = do
  aenv <- readRef' antecedentMap
  return $ Map.lookup (MID.Library mc) aenv
