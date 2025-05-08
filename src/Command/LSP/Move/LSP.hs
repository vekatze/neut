module Command.LSP.Move.LSP
  ( Handle,
    new,
    lsp,
  )
where

import Command.Common.Move.Fetch qualified as Fetch
import Command.LSP.Move.Internal.Server qualified as L
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Kernel.Common.Move.CreateGlobalHandle qualified as Global
import Kernel.Common.Rule.Handle.Global.Env qualified as Env
import Library.Error.Rule.EIO (EIO)

data Handle = Handle
  { fetchHandle :: Fetch.Handle,
    envHandle :: Env.Handle,
    lspHandle :: L.Handle
  }

new ::
  Global.Handle ->
  Handle
new globalHandle = do
  let fetchHandle = Fetch.new globalHandle
  let envHandle = Global.envHandle globalHandle
  let lspHandle = L.new globalHandle
  Handle {..}

lsp :: Handle -> EIO ()
lsp h = do
  Fetch.fetch (fetchHandle h) (Env.getMainModule (envHandle h))
  void $ liftIO $ L.lsp (lspHandle h)
