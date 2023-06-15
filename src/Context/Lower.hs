module Context.Lower
  ( initialize,
    getDefinedNameSet,
  )
where

import Context.App
import Context.App.Internal
import Data.Set qualified as S
import Entity.DefiniteDescription qualified as DD

initialize :: [DD.DefiniteDescription] -> App ()
initialize nameList = do
  writeRef' staticTextList []
  writeRef' definedNameSet $ S.fromList nameList

getDefinedNameSet :: App (S.Set DD.DefiniteDescription)
getDefinedNameSet =
  readRef' definedNameSet
