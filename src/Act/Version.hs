module Act.Version
  ( showVersion,
    Config (..),
  )
where

import qualified Data.Version as V
import Paths_neut

data Config = Config {}

showVersion :: Config -> IO ()
showVersion _ = do
  putStrLn $ V.showVersion version
