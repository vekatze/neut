module Command.Common.Move.SaveModule
  ( Handle,
    new,
    save,
  )
where

import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class
import Data.Text qualified as T
import Kernel.Common.Rule.Module
import Library.Ens.Rule.Ens
import Library.Ens.Rule.Ens.ToDoc qualified as Ens
import Library.Error.Rule.EIO (EIO)
import Library.Logger.Move.Debug qualified as Logger
import Library.Logger.Rule.Handle qualified as Logger
import Library.Path.Move.Write (writeText)
import Path

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
