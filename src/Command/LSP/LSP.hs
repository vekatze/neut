module Command.LSP.LSP (Lsp, lspOptions) where

import Command.LSP.CodeAction qualified as CA
import Language.LSP.Protocol.Types
import Language.LSP.Server

type Lsp config =
  LspT config IO

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
