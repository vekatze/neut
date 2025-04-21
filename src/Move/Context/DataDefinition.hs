module Move.Context.DataDefinition
  ( insert,
    lookup,
  )
where

import Move.Context.App
import Move.Context.App.Internal
import Data.HashMap.Strict qualified as Map
import Rule.Binder
import Rule.DefiniteDescription qualified as DD
import Rule.Discriminant qualified as D
import Rule.Hint
import Rule.IsConstLike
import Rule.Term
import Prelude hiding (lookup, read)

insert ::
  DD.DefiniteDescription ->
  [BinderF Term] ->
  [(SavedHint, DD.DefiniteDescription, IsConstLike, [BinderF Term], D.Discriminant)] ->
  App ()
insert dataName dataArgs consInfoList = do
  let value = map (\(_, _, _, consArgs, discriminant) -> (discriminant, dataArgs, consArgs)) consInfoList
  modifyRef' dataDefMap $ Map.insert dataName value

lookup :: DD.DefiniteDescription -> App (Maybe [(D.Discriminant, [BinderF Term], [BinderF Term])])
lookup dataName = do
  Map.lookup dataName <$> readRef' dataDefMap
