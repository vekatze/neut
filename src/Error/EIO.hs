module Error.EIO (EIO) where

import Control.Monad.Except (ExceptT)
import Error.Error qualified as E

type EIO = ExceptT E.Error IO
