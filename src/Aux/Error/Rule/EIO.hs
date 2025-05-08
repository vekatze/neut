module Aux.Error.Rule.EIO (EIO) where

import Aux.Error.Rule.Error qualified as E
import Control.Monad.Except (ExceptT)

type EIO = ExceptT E.Error IO
