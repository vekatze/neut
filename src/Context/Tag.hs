module Context.Tag
  ( initialize,
    insert,
    get,
  )
where

import Context.App
import Context.App.Internal
import Entity.Hint
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
