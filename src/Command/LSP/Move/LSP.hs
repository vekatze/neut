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
import Error.Rule.EIO (EIO)
import Main.Move.Context.Env qualified as Env
import Main.Move.Scene.Init.Base qualified as Base

data Handle = Handle
  { fetchHandle :: Fetch.Handle,
    envHandle :: Env.Handle,
    lspHandle :: L.Handle
  }

new ::
  Base.Handle ->
  Handle
new baseHandle = do
  let fetchHandle = Fetch.new baseHandle
  let envHandle = Base.envHandle baseHandle
  let lspHandle = L.new baseHandle
  Handle {..}

lsp :: Handle -> EIO ()
lsp h = do
  Fetch.fetch (fetchHandle h) (Env.getMainModule (envHandle h))
  void $ liftIO $ L.lsp (lspHandle h)
