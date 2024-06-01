module Entity.AppLsp (AppLsp, lspOptions) where

import Context.App
import Language.LSP.Protocol.Types
import Language.LSP.Server

type AppLsp config =
  LspT config App

lspOptions :: Options
lspOptions =
  defaultOptions
    { optTextDocumentSync =
        Just
          TextDocumentSyncOptions
            { _openClose = Just True,
              _change = Just TextDocumentSyncKind_Incremental,
              _willSave = Just True,
              _willSaveWaitUntil = Just False,
              _save = Just $ InR $ SaveOptions {_includeText = Just False}
            },
      optCompletionTriggerCharacters = Just ['.']
    }
