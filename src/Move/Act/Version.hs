module Move.Act.Version (showVersion) where

import Data.Version qualified as V
import Paths_neut
import Rule.Config.Version

showVersion :: Config -> IO ()
showVersion _ = do
  putStrLn $ V.showVersion version
