module Move.Context.RawImportSummary
  ( initialize,
    set,
    get,
  )
where

import Move.Context.App
import Move.Context.App.Internal
import Rule.RawImportSummary qualified as RIS
import Rule.RawProgram (RawImport)

initialize :: App ()
initialize =
  writeRef' importEnv Nothing

set :: RawImport -> App ()
set rawImport = do
  writeRef' importEnv $ Just (RIS.fromRawImport rawImport)

get :: App (Maybe RIS.RawImportSummary)
get =
  readRef' importEnv
