module Act.Version
  ( showVersion,
    Config (..),
    Context (..),
  )
where

import Data.Version qualified as V
import Entity.Config.Version
import Paths_neut

class Monad m => Context m where
  printString :: String -> m ()

showVersion :: Context m => Config -> m ()
showVersion _ = do
  printString $ V.showVersion version
