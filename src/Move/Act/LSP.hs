module Move.Act.LSP
  ( Handle,
    new,
    lsp,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Init.Compiler qualified as InitCompiler
import Move.Scene.LSP qualified as L
import Rule.Config.Remark (lspConfig)

data Handle
  = Handle
  { initCompilerHandle :: InitCompiler.Handle,
    fetchHandle :: Fetch.Handle,
    envHandle :: Env.Handle,
    lspHandle :: L.Handle
  }

new ::
  InitCompiler.Handle ->
  Fetch.Handle ->
  Env.Handle ->
  L.Handle ->
  Handle
new initCompilerHandle fetchHandle envHandle lspHandle = do
  Handle {..}

lsp :: Handle -> EIO ()
lsp h = do
  setup h
  void $ liftIO $ L.lsp (lspHandle h)

setup :: Handle -> EIO ()
setup h = do
  InitCompiler.initializeCompiler (initCompilerHandle h) lspConfig
  Env.getMainModule (envHandle h) >>= Fetch.fetch (fetchHandle h)
