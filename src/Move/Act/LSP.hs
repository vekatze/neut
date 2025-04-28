module Move.Act.LSP
  ( Handle,
    new,
    lsp,
  )
where

import Control.Monad
import Move.Context.Antecedent qualified as Antecedent
import Move.Context.App
import Move.Context.Color qualified as Color
import Move.Context.Debug qualified as Debug
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

new ::
  Env.Handle ->
  Gensym.Handle ->
  Color.Handle ->
  Debug.Handle ->
  Locator.Handle ->
  Tag.Handle ->
  Antecedent.Handle ->
  App Handle
new envHandle gensymHandle colorHandle debugHandle locatorHandle tagHandle antecedentHandle = do
  initCompilerHandle <- InitCompiler.new envHandle gensymHandle colorHandle
  fetchHandle <- Fetch.new envHandle gensymHandle colorHandle
  lspHandle <- L.new envHandle gensymHandle colorHandle debugHandle locatorHandle tagHandle antecedentHandle
  return $ Handle {..}

lsp :: Handle -> App ()
lsp h = do
  toApp $ setup h
  void $ L.lsp (lspHandle h)

setup :: Handle -> EIO ()
setup h = do
  InitCompiler.initializeCompiler (initCompilerHandle h) lspConfig
  Env.getMainModule (envHandle h) >>= Fetch.fetch (fetchHandle h)
