module ProgressIndicator.Handle (Handle, InnerHandle (..)) where

import Console.Handle qualified as Console
import Control.Concurrent.MVar
import Data.IORef (IORef)
import Logger.Handle qualified as Logger
import ProgressIndicator.ProgressIndicator (ProgressBar)
import UnliftIO.Async

type Handle =
  Maybe InnerHandle

data InnerHandle = Handle
  { consoleHandle :: Console.Handle,
    loggerHandle :: Logger.Handle,
    progressBarRef :: IORef ProgressBar,
    printLock :: MVar (),
    reportMode :: Console.ReportMode,
    renderThread :: Maybe (Async ())
  }
