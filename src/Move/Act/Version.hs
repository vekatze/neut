module Move.Act.Version (showVersion) where

import Data.Version qualified as V
import Move.Console.Report
import Paths_neut
import Rule.Config.Version

showVersion :: Config -> IO ()
showVersion _ = do
  printString $ V.showVersion version
