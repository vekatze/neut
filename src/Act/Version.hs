module Act.Version
  ( showVersion,
    Config (..),
    Context (..),
  )
where

import Data.Version qualified as V
import Paths_neut

data Config = Config {}

class Monad m => Context m where
  printString :: String -> m ()

showVersion :: Context m => Config -> m ()
showVersion _ = do
  printString $ V.showVersion version
