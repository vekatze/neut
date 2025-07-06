module Command.Version.Version (showVersion) where

import CommandParser.Config.Version
import Data.Version qualified as V
import Paths_neut

showVersion :: Config -> IO ()
showVersion _ = do
  putStrLn $ V.showVersion version
