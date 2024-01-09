module Context.RawImportSummary
  ( initialize,
    set,
    get,
  )
where

import Context.App
import Context.App.Internal
import Entity.RawImportSummary qualified as RIS
import Entity.RawProgram (RawImport)

initialize :: App ()
initialize =
  writeRef' importEnv Nothing

set :: RawImport -> App ()
set rawImport = do
  writeRef' importEnv $ Just (RIS.fromRawImport rawImport)

get :: App (Maybe RIS.RawImportSummary)
get =
  readRef' importEnv
