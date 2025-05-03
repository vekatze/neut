module Move.Scene.Module.Save
  ( Handle,
    new,
    save,
  )
where

import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class
import Data.Text qualified as T
import Logger.Move.Debug qualified as Logger
import Logger.Rule.Handle qualified as Logger
import Move.Context.EIO (EIO)
import Move.Context.Path qualified as Path
import Path
import Rule.Ens
import Rule.Ens.Reify qualified as Ens
import Rule.Module

newtype Handle
  = Handle
  { loggerHandle :: Logger.Handle
  }

new :: Logger.Handle -> Handle
new loggerHandle = do
  Handle {..}

save :: Handle -> Path Abs File -> FullEns -> EIO ()
save h path (c1, (ens, c2)) = do
  liftIO $ Logger.report (loggerHandle h) $ "Saving ens file to: " <> T.pack (toFilePath path)
  ens' <- liftEither $ stylize ens
  liftIO $ Path.writeText path $ Ens.pp (c1, (ens', c2))
