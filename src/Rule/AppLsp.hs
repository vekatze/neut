module Rule.AppLsp (AppLsp, lspOptions) where

import Context.App
import Rule.CodeAction qualified as CA
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
              _willSave = Just False,
              _willSaveWaitUntil = Just False,
              _save = Just $ InR $ SaveOptions {_includeText = Just False}
            },
      optCompletionTriggerCharacters = Just ['.'],
      optExecuteCommandCommands =
        Just
          [ CA.minimizeImportsCommandName,
            CA.refreshCacheCommandName
          ]
    }
