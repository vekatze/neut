module Move.Scene.Module.Save
  ( Handle,
    new,
    save,
  )
where

import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class
import Data.Text qualified as T
import Move.Context.App
import Move.Context.Color qualified as Color
import Move.Context.Debug qualified as Debug
import Move.Context.EIO (EIO)
import Move.Context.Path qualified as Path
import Path
import Rule.Ens
import Rule.Ens.Reify qualified as Ens
import Rule.Module

newtype Handle
  = Handle
  { debugHandle :: Debug.Handle
  }

new :: Color.Handle -> App Handle
new colorHandle = do
  debugHandle <- Debug.new colorHandle
  return $ Handle {..}

save :: Handle -> Path Abs File -> FullEns -> EIO ()
save h path (c1, (ens, c2)) = do
  Debug.report (debugHandle h) $ "Saving ens file to: " <> T.pack (toFilePath path)
  ens' <- liftEither $ stylize ens
  liftIO $ Path.writeText path $ Ens.pp (c1, (ens', c2))
