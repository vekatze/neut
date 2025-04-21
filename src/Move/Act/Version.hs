module Move.Act.Version (showVersion) where

import Move.Context.App
import Move.Context.Remark qualified as Remark
import Data.Version qualified as V
import Rule.Config.Version
import Paths_neut

showVersion :: Config -> App ()
showVersion _ = do
  Remark.printString $ V.showVersion version
