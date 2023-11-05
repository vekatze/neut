module Context.Tag
  ( initialize,
    insert,
    insertBinder,
    insertDD,
    get,
  )
where

import Context.App
import Context.App.Internal
import Data.Text qualified as T
import Entity.Binder
import Entity.DefiniteDescription qualified as DD
import Entity.Hint
import Entity.Ident
import Entity.LocationTree qualified as LT
import Prelude hiding (lookup, read)

initialize :: App ()
initialize =
  writeRef' tagMap LT.empty

insert :: Hint -> Int -> Hint -> App ()
insert mUse nameLength mDef = do
  let (l, c) = metaLocation mUse
  modifyRef' tagMap $ LT.insert (l, (c, c + nameLength)) mDef

get :: App LT.LocationTree
get = do
  readRef' tagMap

insertBinder :: BinderF a -> App ()
insertBinder (m, I (x, _), _) =
  insert m (T.length x) m

insertDD :: Hint -> DD.DefiniteDescription -> App ()
insertDD m dd =
  insert m (T.length (DD.localLocator dd)) m
