module Kernel.Common.Module.GetDependency
  ( Handle (..),
    new,
    getDependency,
  )
where

import App.App (App)
import Kernel.Common.Handle.Global.Antecedent qualified as Antecedent
import Kernel.Common.Handle.Global.Module qualified as Module
import Kernel.Common.Module qualified as M
import Kernel.Common.Module.GetModule qualified as GetModule
import Kernel.Common.Source.ShiftToLatest qualified as STL
import Language.Common.ModuleAlias qualified as MA
import Language.Common.ModuleID qualified as MID
import Logger.Hint qualified as H

data Handle = Handle
  { getModuleHandle :: GetModule.Handle,
    shiftToLatestHandle :: STL.Handle
  }

new :: Module.Handle -> Antecedent.Handle -> Handle
new moduleHandle antecedentHandle = do
  let getModuleHandle = GetModule.new moduleHandle
  let shiftToLatestHandle = STL.new antecedentHandle
  Handle {..}

getDependency :: Handle -> H.Hint -> MA.ModuleAlias -> M.Dependency -> App M.Module
getDependency h m alias dependency = do
  let moduleID = MID.Library $ M.dependencyDigest dependency
  dependencyModule <- GetModule.getModule (getModuleHandle h) m moduleID (MA.reify alias)
  STL.shiftToLatestModule (shiftToLatestHandle h) dependencyModule
