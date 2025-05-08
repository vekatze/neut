module Kernel.Common.Rule.Handle.Local.Locator
  ( Handle (..),
  )
where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text qualified as T
import Kernel.Common.Rule.Handle.Global.Env qualified as Env
import Kernel.Common.Rule.Handle.Local.Tag qualified as Tag
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.LocalLocator qualified as LL
import Language.Common.Rule.StrictGlobalLocator qualified as SGL
import Path

-- the structure of a name of a global variable:
--
--     some.path.to.item.some-function
--     ----------------- -------------
--     ↑ global locator  ↑ local locator
--     ------------------------------------------------
--     ↑ the definite description of a global variable `some-function` (up-to module alias)

data Handle = Handle
  { _tagHandle :: Tag.Handle,
    _envHandle :: Env.Handle,
    _activeDefiniteDescriptionListRef :: IORef (Map.HashMap LL.LocalLocator DD.DefiniteDescription),
    _activeStaticFileListRef :: IORef (Map.HashMap T.Text (Path Abs File, T.Text)),
    _activeGlobalLocatorListRef :: IORef [SGL.StrictGlobalLocator],
    _currentGlobalLocatorRef :: IORef (Maybe SGL.StrictGlobalLocator)
  }
