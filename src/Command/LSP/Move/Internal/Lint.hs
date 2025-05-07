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
import Kernel.Move.Context.Env qualified as Env
import Kernel.Move.Scene.Init.Base qualified as Base
import Language.LSP.Server

newtype Handle = Handle
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
