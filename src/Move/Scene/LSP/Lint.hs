module Move.Scene.LSP.Lint
  ( Handle,
    new,
    lint,
  )
where

import Control.Monad
import Language.LSP.Server
import Move.Context.EIO (EIO)
import Move.Context.Env qualified as Env
import Move.Scene.Check qualified as Check
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.LSP.Util (maxDiagNum, report, runOneShot)
import Move.Scene.LSP.Util qualified as LspUtil
import Rule.Lsp
import Rule.Remark qualified as R

data Handle
  = Handle
  { fetchHandle :: Fetch.Handle,
    envHandle :: Env.Handle,
    checkHandle :: Check.Handle,
    lspUtilHandle :: LspUtil.Handle
  }

new :: Fetch.Handle -> Env.Handle -> Check.Handle -> LspUtil.Handle -> Handle
new fetchHandle envHandle checkHandle lspUtilHandle = do
  Handle {..}

lint :: Handle -> Lsp () ()
lint h = do
  flushDiagnosticsBySource maxDiagNum (Just "neut")
  remarksOrNone <- runOneShot (lspUtilHandle h) (lintM h)
  forM_ remarksOrNone report

lintM :: Handle -> EIO [R.Remark]
lintM h = do
  Env.getMainModule (envHandle h)
    >>= Fetch.fetch (fetchHandle h)
    >> Check.check (checkHandle h)
