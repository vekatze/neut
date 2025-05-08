module Library.Error.Rule.EIO (EIO) where

import Control.Monad.Except (ExceptT)
import Library.Error.Rule.Error qualified as E

type EIO = ExceptT E.Error IO
