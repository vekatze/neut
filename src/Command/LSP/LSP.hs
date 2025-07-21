module Command.LSP.LSP
  ( Handle,
    new,
    lsp,
  )
where

import App.App (App)
import Command.Common.Fetch qualified as Fetch
import Command.LSP.Internal.Server qualified as L
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env

data Handle = Handle
  { fetchHandle :: Fetch.Handle,
    envHandle :: Env.Handle
  }

new ::
  Global.Handle ->
  Handle
new globalHandle = do
  let fetchHandle = Fetch.new globalHandle
  let envHandle = Global.envHandle globalHandle
  Handle {..}

lsp :: Handle -> App ()
lsp h = do
  Fetch.fetch (fetchHandle h) (Env.getMainModule (envHandle h))
  void $ liftIO L.lsp
