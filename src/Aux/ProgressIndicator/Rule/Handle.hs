module Aux.ProgressIndicator.Rule.Handle (Handle, InnerHandle (..)) where

import Aux.Color.Rule.Handle qualified as Color
import Aux.ProgressIndicator.Rule.ProgressIndicator (ProgressBar)
import Data.IORef (IORef)
import UnliftIO.Async

type Handle =
  Maybe InnerHandle

data InnerHandle = Handle
  { colorHandle :: Color.Handle,
    progressBarRef :: IORef ProgressBar,
    renderThread :: Async ()
  }
