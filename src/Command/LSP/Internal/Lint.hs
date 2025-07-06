module Command.LSP.Internal.Lint
  ( Handle,
    new,
    lint,
  )
where

import Command.Common.Check qualified as Check
import Command.Common.Fetch qualified as Fetch
import Command.LSP.Internal.Util (maxDiagNum, report, run)
import Command.LSP.LSP
import Control.Monad
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
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
