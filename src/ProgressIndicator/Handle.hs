module ProgressIndicator.Handle (Handle, InnerHandle (..)) where

import Color.Handle qualified as Color
import Data.IORef (IORef)
import ProgressIndicator.ProgressIndicator (ProgressBar)
import UnliftIO.Async

type Handle =
  Maybe InnerHandle

data InnerHandle = Handle
  { colorHandle :: Color.Handle,
    progressBarRef :: IORef ProgressBar,
    renderThread :: Async ()
  }
