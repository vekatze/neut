module Move.Scene.LSP.Lint
  ( Handle,
    new,
    lint,
  )
where

import Control.Monad
import Control.Monad.Trans
import Language.LSP.Server
import Move.Context.App (App)
import Move.Context.AppM (AppM)
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Scene.Check qualified as Check
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.Init.Compiler qualified as InitCompiler
import Move.Scene.LSP.Util (liftAppM, maxDiagNum, report)
import Rule.AppLsp
import Rule.Remark qualified as R

data Handle
  = Handle
  { fetchHandle :: Fetch.Handle,
    envHandle :: Env.Handle,
    initCompilerHandle :: InitCompiler.Handle,
    checkHandle :: Check.Handle
  }

new :: Fetch.Handle -> Env.Handle -> InitCompiler.Handle -> Check.Handle -> App Handle
new fetchHandle envHandle initCompilerHandle checkHandle = do
  return $ Handle {..}

lint :: Handle -> AppLsp () ()
lint h = do
  flushDiagnosticsBySource maxDiagNum (Just "neut")
  remarksOrNone <- liftAppM (initCompilerHandle h) $ lintM h
  forM_ remarksOrNone report

lintM :: Handle -> AppM [R.Remark]
lintM h = do
  lift $
    toApp (Env.getMainModule (envHandle h))
      >>= toApp . Fetch.fetch (fetchHandle h)
      >> Check.check (checkHandle h)
