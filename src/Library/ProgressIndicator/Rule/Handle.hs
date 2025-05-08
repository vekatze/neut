module Library.ProgressIndicator.Rule.Handle (Handle, InnerHandle (..)) where

import Data.IORef (IORef)
import Library.Color.Rule.Handle qualified as Color
import Library.ProgressIndicator.Rule.ProgressIndicator (ProgressBar)
import UnliftIO.Async

type Handle =
  Maybe InnerHandle

data InnerHandle = Handle
  { colorHandle :: Color.Handle,
    progressBarRef :: IORef ProgressBar,
    renderThread :: Async ()
  }
