module Command.LSP.Internal.Lint
  ( lint,
  )
where

import App.App (App)
import App.Error qualified as E
import App.Run (runApp)
import Command.Common.Check qualified as Check
import Command.Common.Fetch qualified as Fetch
import Command.LSP.Internal.DiagnosticStore qualified as DiagnosticStore
import Command.LSP.Internal.DocumentStateStore qualified as DocumentStateStore
import Command.LSP.Internal.LintWorker qualified as LintWorker
import Command.LSP.Internal.Source.Reflect qualified as SourceReflect
import Command.LSP.Internal.Util (Lsp, LspState (..), maxDiagNum, report, reportForFile)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Kernel.Common.CreateGlobalHandle qualified as Global
import Kernel.Common.Handle.Global.Env qualified as Env
import Kernel.Common.Handle.Global.GlobalRemark qualified as GlobalRemark
import Kernel.Common.Source (Source (..))
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Logger.Log (Log)

lint :: LspState -> Uri -> Lsp () ()
lint lspState uri = do
  lintFile lspState uri
  lintModule lspState

lintFile :: LspState -> Uri -> Lsp () ()
lintFile lspState uri =
  forM_ (uriToFilePath uri) $ \path ->
    withGlobalHandle lspState $ \h -> do
      logs <- collectFileLogs h $ do
        fetch h
        sourceOrNone <- SourceReflect.reflect (SourceReflect.new h) path
        forM_ sourceOrNone $ \source ->
          Check.checkSingle (Check.new h) (sourceModule source) (sourceFilePath source)
      liftIO $ DocumentStateStore.refreshDocumentStates h (documentStateStore lspState)
      liftIO $ LintWorker.markFresh (lintWorker lspState) (toNormalizedUri uri)
      reportForFile lspState (toNormalizedUri uri) logs

lintModule :: LspState -> Lsp () ()
lintModule lspState =
  withGlobalHandle lspState $ \h -> do
    logs <- collectModuleLogs h $ do
      fetch h
      Check.check (Check.new h) Nothing
    liftIO $ DocumentStateStore.refreshDocumentStates h (documentStateStore lspState)
    flushDiagnosticsBySource maxDiagNum (Just "neut")
    liftIO $ DiagnosticStore.clear (diagnosticStore lspState)
    report lspState logs

fetch :: Global.Handle -> App ()
fetch h =
  Fetch.fetch (Fetch.new h) (Env.getMainModule (Global.envHandle h))

collectFileLogs :: Global.Handle -> App a -> Lsp () [Log]
collectFileLogs h comp = do
  resultOrError <- liftIO $ runApp comp
  remarks <- liftIO $ GlobalRemark.get (Global.globalRemarkHandle h)
  case resultOrError of
    Left (E.MakeError errors) ->
      return $ errors ++ remarks
    Right _ ->
      return remarks

collectModuleLogs :: Global.Handle -> App [Log] -> Lsp () [Log]
collectModuleLogs h comp = do
  resultOrError <- liftIO $ runApp comp
  case resultOrError of
    Left (E.MakeError errors) -> do
      remarks <- liftIO $ GlobalRemark.get (Global.globalRemarkHandle h)
      return $ errors ++ remarks
    Right logs ->
      return logs

withGlobalHandle :: LspState -> (Global.Handle -> Lsp () ()) -> Lsp () ()
withGlobalHandle lspState cont = do
  handleOrError <- liftIO $ Global.newOrError (consoleHandle lspState) (loggerHandle lspState) (localArchiveMap lspState) Nothing Nothing
  case handleOrError of
    Left (E.MakeError errors) ->
      report lspState errors
    Right h ->
      cont h
