module Context.KeyArg
  ( insert,
    lookup,
  )
where

import Context.App
import Context.App.Internal
import Context.Remark (printNote')
import Context.Throw qualified as Throw
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Entity.Arity qualified as A
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.Key
import Prelude hiding (lookup, read)

insert :: DD.DefiniteDescription -> A.Arity -> [Key] -> App ()
insert funcName arity keys = do
  modifyRef' keyArgMap $ Map.insert funcName (arity, keys)

lookup :: Hint -> DD.DefiniteDescription -> App (A.Arity, [Key])
lookup m dataName = do
  mValue <- Map.lookup dataName <$> readRef' keyArgMap
  case mValue of
    Just value ->
      return value
    Nothing -> do
      printNote' $ T.pack (show dataName)
      Throw.raiseError m $ "no such function is defined: " <> DD.reify dataName
