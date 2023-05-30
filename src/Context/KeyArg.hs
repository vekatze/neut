module Context.KeyArg
  ( insert,
    lookup,
    lookupMaybe,
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
import Entity.IsConstLike
import Entity.Key
import Prelude hiding (lookup, read)

insert :: DD.DefiniteDescription -> IsConstLike -> A.Arity -> [Key] -> App ()
insert funcName isConstLike arity keys = do
  modifyRef' keyArgMap $ Map.insert funcName (isConstLike, (arity, keys))

lookup :: Hint -> DD.DefiniteDescription -> App (A.Arity, [Key])
lookup m dataName = do
  mValue <- Map.lookup dataName <$> readRef' keyArgMap
  case mValue of
    Just (_, value) ->
      return value
    Nothing -> do
      printNote' $ T.pack (show dataName)
      Throw.raiseError m $ "no such function is defined: " <> DD.reify dataName

lookupMaybe :: DD.DefiniteDescription -> App (Maybe (IsConstLike, [Key]))
lookupMaybe dataName = do
  mValue <- Map.lookup dataName <$> readRef' keyArgMap
  case mValue of
    Just (isConstLike, (_, keyList)) ->
      return $ Just (isConstLike, keyList)
    _ ->
      return Nothing
