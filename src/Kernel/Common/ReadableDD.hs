module Kernel.Common.ReadableDD
  ( readableDD,
    readableDD',
  )
where

import Data.Text qualified as T
import Kernel.Common.Handle.Global.ModulePath qualified as ModulePath
import Kernel.Common.Module hiding (moduleID)
import Language.Common.DefiniteDescription qualified as DD

readableDD :: MainModule -> DD.DefiniteDescription -> T.Text
readableDD mainModule = do
  readableDD' (extractModule mainModule)

readableDD' :: Module -> DD.DefiniteDescription -> T.Text
readableDD' baseModule =
  readableDDWith (ModulePath.directModulePathMap baseModule)

readableDDWith :: ModulePath.ModulePathMap -> DD.DefiniteDescription -> T.Text
readableDDWith =
  ModulePath.renderDD
