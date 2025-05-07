module Kernel.Move.Scene.Module.Save
  ( Handle,
    new,
    save,
  )
where

import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class
import Data.Text qualified as T
import Ens.Rule.Ens
import Ens.Rule.Ens.ToDoc qualified as Ens
import Error.Rule.EIO (EIO)
import Kernel.Rule.Module
import Logger.Move.Debug qualified as Logger
import Logger.Rule.Handle qualified as Logger
import Path
import Path.Move.Write (writeText)

newtype Handle = Handle
  { loggerHandle :: Logger.Handle
  }

new :: Logger.Handle -> Handle
new loggerHandle = do
  Handle {..}

save :: Handle -> Path Abs File -> FullEns -> EIO ()
save h path (c1, (ens, c2)) = do
  liftIO $ Logger.report (loggerHandle h) $ "Saving ens file to: " <> T.pack (toFilePath path)
  ens' <- liftEither $ stylize ens
  liftIO $ writeText path $ Ens.pp (c1, (ens', c2))
