module Scene.LSP.Lint (lint) where

import Context.AppM (AppM)
import Context.Env qualified as Env
import Control.Monad
import Control.Monad.Trans
import Rule.AppLsp
import Rule.Remark qualified as R
import Language.LSP.Server
import Scene.Check qualified as Check
import Scene.Fetch qualified as Fetch
import Scene.LSP.Util (liftAppM, maxDiagNum, report)

lint :: AppLsp () ()
lint = do
  flushDiagnosticsBySource maxDiagNum (Just "neut")
  remarksOrNone <- liftAppM lintM
  forM_ remarksOrNone report

lintM :: AppM [R.Remark]
lintM = do
  lift $ Env.getMainModule >>= Fetch.fetch >> Check.check
