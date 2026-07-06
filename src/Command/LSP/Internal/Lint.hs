module Command.LSP.Internal.Lint
  ( Handle,
    new,
    lint,
  )
where

import Command.Common.Check qualified as Check
import Command.Common.Fetch qualified as Fetch
import Command.LSP.Internal.DiagnosticStore qualified as DiagnosticStore
import Command.LSP.Internal.DocumentStateStore qualified as DocumentStateStore
import Command.LSP.Internal.Util (Lsp, LspState (..), maxDiagNum, report, run)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Language.LSP.Server

data Handle = Handle
  { globalHandle :: Global.Handle,
    lspState :: LspState
  }

new :: Global.Handle -> LspState -> Handle
new globalHandle lspState = do
  Handle {..}

lint :: Handle -> Lsp () ()
lint h = do
  let fetchHandle = Fetch.new (globalHandle h)
  let envHandle = Global.envHandle (globalHandle h)
  let checkHandle = Check.new (globalHandle h)
  flushDiagnosticsBySource maxDiagNum (Just "neut")
  liftIO $ DiagnosticStore.clear (diagnosticStore (lspState h))
  remarksOrNone <- run (lspState h) (globalHandle h) $ do
    Fetch.fetch fetchHandle (Env.getMainModule envHandle)
    Check.check checkHandle
  liftIO $ DocumentStateStore.refreshDocumentStates (globalHandle h) (documentStateStore (lspState h))
  forM_ remarksOrNone (report (lspState h))
