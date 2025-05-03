module Main.Move.Scene.LSP.Lint
  ( Handle,
    new,
    lint,
  )
where

import Control.Monad
import Language.LSP.Server
import Main.Move.Context.Env qualified as Env
import Main.Move.Scene.Check qualified as Check
import Main.Move.Scene.Fetch qualified as Fetch
import Main.Move.Scene.Init.Base qualified as Base
import Main.Move.Scene.LSP.Util (maxDiagNum, report, run)
import Main.Rule.Lsp

newtype Handle
  = Handle
  { baseHandle :: Base.Handle
  }

new :: Base.Handle -> Handle
new baseHandle = do
  Handle {..}

lint :: Handle -> Lsp () ()
lint h = do
  let fetchHandle = Fetch.new (baseHandle h)
  let envHandle = Base.envHandle (baseHandle h)
  let checkHandle = Check.new (baseHandle h)
  flushDiagnosticsBySource maxDiagNum (Just "neut")
  remarksOrNone <- run (baseHandle h) $ do
    Fetch.fetch fetchHandle (Env.getMainModule envHandle)
    Check.check checkHandle
  forM_ remarksOrNone report
