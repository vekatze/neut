module Act.Version (showVersion) where

import Context.App
import Context.Remark qualified as Remark
import Data.Version qualified as V
import Entity.Config.Version
import Paths_neut

showVersion :: Config -> App ()
showVersion _ = do
  Remark.printString $ V.showVersion version
