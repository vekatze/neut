module Context.Antecedent where

import Context.App
import Context.App.Internal
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Entity.Module qualified as M
import Entity.ModuleChecksum qualified as MC
import Prelude hiding (lookup, read)

insert :: MC.ModuleChecksum -> M.Module -> App ()
insert old new =
  modifyRef' antecedentMap $ Map.insert old new

lookup :: MC.ModuleChecksum -> App (Maybe M.Module)
lookup mc = do
  aenv <- readRef' antecedentMap
  return $ Map.lookup mc aenv
