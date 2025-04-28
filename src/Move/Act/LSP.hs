module Move.Act.LSP
  ( Handle,
    new,
    lsp,
  )
where

import Control.Monad
import Move.Context.App
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Language.Utility.Gensym qualified as Gensym
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

new :: Gensym.Handle -> App Handle
new gensymHandle = do
  initCompilerHandle <- InitCompiler.new
  fetchHandle <- Fetch.new
  envHandle <- Env.new
  lspHandle <- L.new gensymHandle
  return $ Handle {..}

lsp :: Handle -> App ()
lsp h = do
  setup h
  void $ L.lsp (lspHandle h)

setup :: Handle -> App ()
setup h = do
  toApp $ InitCompiler.initializeCompiler (initCompilerHandle h) lspConfig
  toApp $ Env.getMainModule (envHandle h) >>= Fetch.fetch (fetchHandle h)
