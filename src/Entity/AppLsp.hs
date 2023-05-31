module Entity.AppLsp (AppLsp, lspOptions) where

import Context.App
import Language.LSP.Server
import Language.LSP.Types

type AppLsp config =
  LspT config App

lspOptions :: Options
lspOptions =
  defaultOptions
    { textDocumentSync =
        Just
          TextDocumentSyncOptions
            { _openClose = Just True,
              _change = Just TdSyncNone,
              _willSave = Just False,
              _willSaveWaitUntil = Just False,
              _save = Just $ InR $ SaveOptions $ Just False
            }
    }
