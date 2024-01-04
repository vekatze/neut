module Context.Lower
  ( initialize,
    getDefinedNameSet,
    insDefinedName,
  )
where

import Context.App
import Context.App.Internal
import Data.Set qualified as S
import Entity.DefiniteDescription qualified as DD

initialize :: App ()
initialize = do
  writeRef' staticTextList []
  writeRef' definedNameSet S.empty

getDefinedNameSet :: App (S.Set DD.DefiniteDescription)
getDefinedNameSet =
  readRef' definedNameSet

insDefinedName :: DD.DefiniteDescription -> App ()
insDefinedName dd =
  modifyRef' definedNameSet $ S.insert dd
