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
import Move.Language.Utility.Gensym qualified as Gensym
import Move.Scene.Check qualified as Check
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.LSP.Util (liftAppM, maxDiagNum, report)
import Rule.AppLsp
import Rule.Remark qualified as R

data Handle
  = Handle
  { fetchHandle :: Fetch.Handle,
    envHandle :: Env.Handle,
    checkHandle :: Check.Handle,
    gensymHandle :: Gensym.Handle
  }

new :: Gensym.Handle -> App Handle
new gensymHandle = do
  fetchHandle <- Fetch.new gensymHandle
  envHandle <- Env.new
  checkHandle <- Check.new gensymHandle
  return $ Handle {..}

lint :: Handle -> AppLsp () ()
lint h = do
  flushDiagnosticsBySource maxDiagNum (Just "neut")
  remarksOrNone <- liftAppM (gensymHandle h) $ lintM h
  forM_ remarksOrNone report

lintM :: Handle -> AppM [R.Remark]
lintM h = do
  lift $
    toApp (Env.getMainModule (envHandle h))
      >>= toApp . Fetch.fetch (fetchHandle h)
      >> Check.check (checkHandle h)
