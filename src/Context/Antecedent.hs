module Context.Antecedent where

import Context.App
import Context.App.Internal
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Entity.Module qualified as M
import Entity.ModuleDigest qualified as MD
import Prelude hiding (lookup, read)

insert :: MD.ModuleDigest -> M.Module -> App ()
insert old new =
  modifyRef' antecedentMap $ Map.insert old new

lookup :: MD.ModuleDigest -> App (Maybe M.Module)
lookup mc = do
  aenv <- readRef' antecedentMap
  return $ Map.lookup mc aenv
