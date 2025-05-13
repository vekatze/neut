module Error.Rule.EIO (EIO) where

import Error.Rule.Error qualified as E
import Control.Monad.Except (ExceptT)

type EIO = ExceptT E.Error IO
