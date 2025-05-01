module Move.Scene.LSP.Lint
  ( Handle,
    new,
    lint,
  )
where

import Control.Monad
import Language.LSP.Server
import Move.Context.Env qualified as Env
import Move.Scene.Check qualified as Check
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Init.Base qualified as Base
import Move.Scene.LSP.Util (maxDiagNum, report, run)
import Rule.Lsp

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
