module Context.CodataDefinition
  ( insert,
    lookup,
  )
where

import Context.App
import Context.App.Internal
import Context.Throw qualified as Throw
import Data.HashMap.Strict qualified as Map
import Entity.Arity qualified as A
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Prelude hiding (lookup, read)

insert :: DD.DefiniteDescription -> (DD.DefiniteDescription, A.Arity, A.Arity) -> [DD.DefiniteDescription] -> App ()
insert dataName dataNewInfo consNameList = do
  modifyRef' codataDefMap $ Map.insert dataName (dataNewInfo, consNameList)

lookup :: Hint -> DD.DefiniteDescription -> App ((DD.DefiniteDescription, A.Arity, A.Arity), [DD.DefiniteDescription])
lookup m dataName = do
  mValue <- Map.lookup dataName <$> readRef' codataDefMap
  case mValue of
    Just value ->
      return value
    Nothing -> do
      Throw.raiseError m $ "no such codata is defined: " <> DD.reify dataName
