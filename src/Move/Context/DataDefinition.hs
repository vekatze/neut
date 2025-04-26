module Move.Context.DataDefinition
  ( Handle,
    new,
    insert',
    lookup',
  )
where

import Control.Monad.Reader (asks)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Move.Context.App
import Move.Context.App.Internal qualified as App
import Rule.Binder
import Rule.DefiniteDescription qualified as DD
import Rule.Discriminant qualified as D
import Rule.Hint
import Rule.IsConstLike
import Rule.Term
import Prelude hiding (lookup, read)

newtype Handle
  = Handle
  { dataDefMapRef :: IORef (Map.HashMap DD.DefiniteDescription [(D.Discriminant, [BinderF Term], [BinderF Term])])
  }

new :: App Handle
new = do
  dataDefMapRef <- asks App.dataDefMap
  return $ Handle {..}

insert' ::
  Handle ->
  DD.DefiniteDescription ->
  [BinderF Term] ->
  [(SavedHint, DD.DefiniteDescription, IsConstLike, [BinderF Term], D.Discriminant)] ->
  IO ()
insert' h dataName dataArgs consInfoList = do
  let value = map (\(_, _, _, consArgs, discriminant) -> (discriminant, dataArgs, consArgs)) consInfoList
  modifyIORef' (dataDefMapRef h) $ Map.insert dataName value

lookup' :: Handle -> DD.DefiniteDescription -> IO (Maybe [(D.Discriminant, [BinderF Term], [BinderF Term])])
lookup' h dataName = do
  Map.lookup dataName <$> readIORef (dataDefMapRef h)
