module Move.Act.LSP
  ( Handle,
    new,
    lsp,
  )
where

import Control.Monad
import Move.Context.Antecedent qualified as Antecedent
import Move.Context.App
import Move.Context.EIO (EIO, toApp)
import Move.Context.Env qualified as Env
import Move.Context.Locator qualified as Locator
import Move.Context.Tag qualified as Tag
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

new :: Env.Handle -> Gensym.Handle -> Locator.Handle -> Tag.Handle -> Antecedent.Handle -> App Handle
new envHandle gensymHandle locatorHandle tagHandle antecedentHandle = do
  initCompilerHandle <- InitCompiler.new envHandle gensymHandle
  fetchHandle <- Fetch.new envHandle gensymHandle
  lspHandle <- L.new envHandle gensymHandle locatorHandle tagHandle antecedentHandle
  return $ Handle {..}

lsp :: Handle -> App ()
lsp h = do
  toApp $ setup h
  void $ L.lsp (lspHandle h)

setup :: Handle -> EIO ()
setup h = do
  InitCompiler.initializeCompiler (initCompilerHandle h) lspConfig
  Env.getMainModule (envHandle h) >>= Fetch.fetch (fetchHandle h)
