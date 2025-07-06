module Command.LSP.CodeAction
  ( minimizeImportsCommandTitle,
    minimizeImportsCommandName,
    minimizeImportsCommand,
    refreshCacheCommandTitle,
    refreshCacheCommandName,
    refreshCacheCommand,
  )
where

import Data.Aeson.Types qualified as A
import Data.Text qualified as T
import Language.LSP.Protocol.Types

minimizeImportsCommandTitle :: T.Text
minimizeImportsCommandTitle =
  "Minimize imports"

minimizeImportsCommandName :: T.Text
minimizeImportsCommandName =
  "minimizeImports"

minimizeImportsCommand :: Uri -> Command
minimizeImportsCommand uri =
  Command
    { _title = minimizeImportsCommandTitle,
      _command = minimizeImportsCommandName,
      _arguments = Just [A.String (getUri uri)]
    }

refreshCacheCommandTitle :: T.Text
refreshCacheCommandTitle =
  "Refresh completion cache"

refreshCacheCommandName :: T.Text
refreshCacheCommandName =
  "refreshCache"

refreshCacheCommand :: Command
refreshCacheCommand =
  Command
    { _title = refreshCacheCommandTitle,
      _command = refreshCacheCommandName,
      _arguments = Nothing
    }
