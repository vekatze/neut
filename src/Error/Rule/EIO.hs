module Error.Rule.EIO (EIO) where

import Control.Monad.Except (ExceptT)
import Error.Rule.Error qualified as E

type EIO = ExceptT E.Error IO
