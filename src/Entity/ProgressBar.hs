module Entity.ProgressBar (ProgressBar (..)) where

import Data.IORef (IORef)
import Data.Text qualified as T
import System.Console.ANSI (SGR)

data ProgressBar
  = ProgressBar
  { workingTitle :: T.Text,
    completedTitle :: T.Text,
    color :: Maybe [SGR],
    progress :: Maybe (IORef Int, Int)
  }
