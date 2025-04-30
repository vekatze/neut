module Move.Scene.LSP.Lint
  ( Handle,
    new,
    lint,
  )
where

import Control.Monad
import Control.Monad.Trans
import Language.LSP.Server
import Move.Context.AppM qualified as AppM
import Move.Context.EIO (EIO, toApp)
import Move.Context.Env qualified as Env
import Move.Scene.Check qualified as Check
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.LSP.Util (liftAppM, maxDiagNum, report)
import Rule.AppLsp
import Rule.Remark qualified as R

data Handle
  = Handle
  { fetchHandle :: Fetch.Handle,
    envHandle :: Env.Handle,
    appHandle :: AppM.Handle,
    checkHandle :: Check.Handle
  }

new :: Fetch.Handle -> Env.Handle -> AppM.Handle -> Check.Handle -> Handle
new fetchHandle envHandle appHandle checkHandle = do
  Handle {..}

lint :: Handle -> AppLsp () ()
lint h = do
  flushDiagnosticsBySource maxDiagNum (Just "neut")
  remarksOrNone <- liftAppM (appHandle h) $ lift (toApp $ lintM h)
  forM_ remarksOrNone report

lintM :: Handle -> EIO [R.Remark]
lintM h = do
  Env.getMainModule (envHandle h)
    >>= Fetch.fetch (fetchHandle h)
    >> Check.check (checkHandle h)
