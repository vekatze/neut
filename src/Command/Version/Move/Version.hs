module Command.Version.Move.Version (showVersion) where

import Aux.CommandParser.Rule.Config.Version
import Data.Version qualified as V
import Paths_neut

showVersion :: Config -> IO ()
showVersion _ = do
  putStrLn $ V.showVersion version
