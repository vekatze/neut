module Move.Scene.LSP.Lint (lint) where

import Control.Monad
import Control.Monad.Trans
import Language.LSP.Server
import Move.Context.AppM (AppM)
import Move.Context.EIO (toApp)
import Move.Context.Env qualified as Env
import Move.Scene.Check qualified as Check
import Move.Scene.Fetch qualified as Fetch
import Move.Scene.LSP.Util (liftAppM, maxDiagNum, report)
import Rule.AppLsp
import Rule.Remark qualified as R

lint :: AppLsp () ()
lint = do
  flushDiagnosticsBySource maxDiagNum (Just "neut")
  remarksOrNone <- liftAppM lintM
  forM_ remarksOrNone report

lintM :: AppM [R.Remark]
lintM = do
  h <- lift Fetch.new
  lift $ Env.getMainModule >>= toApp . Fetch.fetch h >> Check.check
