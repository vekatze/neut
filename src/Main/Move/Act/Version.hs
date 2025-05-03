module Main.Move.Act.Version (showVersion) where

import Data.Version qualified as V
import Main.Rule.Config.Version
import Paths_neut

showVersion :: Config -> IO ()
showVersion _ = do
  putStrLn $ V.showVersion version
