module Command.LSP.Move.Internal.Lint
  ( Handle,
    new,
    lint,
  )
where

import Command.Common.Move.Check qualified as Check
import Command.Common.Move.Fetch qualified as Fetch
import Command.LSP.Move.Internal.Util (maxDiagNum, report, run)
import Command.LSP.Rule.Lsp
import Control.Monad
import Kernel.Move.Context.Global.Env qualified as Env
import Kernel.Move.Scene.Init.Global qualified as Global
import Language.LSP.Server

newtype Handle = Handle
  { globalHandle :: Global.Handle
  }

new :: Global.Handle -> Handle
new globalHandle = do
  Handle {..}

lint :: Handle -> Lsp () ()
lint h = do
  let fetchHandle = Fetch.new (globalHandle h)
  let envHandle = Global.envHandle (globalHandle h)
  let checkHandle = Check.new (globalHandle h)
  flushDiagnosticsBySource maxDiagNum (Just "neut")
  remarksOrNone <- run (globalHandle h) $ do
    Fetch.fetch fetchHandle (Env.getMainModule envHandle)
    Check.check checkHandle
  forM_ remarksOrNone report
