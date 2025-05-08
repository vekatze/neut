module Command.Version.Move.Version (showVersion) where

import Data.Version qualified as V
import Library.CommandParser.Rule.Config.Version
import Paths_neut

showVersion :: Config -> IO ()
showVersion _ = do
  putStrLn $ V.showVersion version
