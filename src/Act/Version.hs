module Act.Version (showVersion) where

import Context.App
import Context.Log qualified as Log
import Data.Version qualified as V
import Entity.Config.Version
import Paths_neut

showVersion :: Config -> App ()
showVersion _ = do
  Log.printString $ V.showVersion version
