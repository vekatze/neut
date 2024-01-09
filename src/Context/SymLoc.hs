module Context.SymLoc
  ( initialize,
    insert,
    get,
  )
where

import Context.App
import Context.App.Internal
import Control.Monad (unless)
import Entity.Hint
import Entity.Ident
import Entity.LocalVarTree qualified as LVT

initialize :: App ()
initialize =
  writeRef' localVarMap LVT.empty

insert :: Ident -> Loc -> Loc -> App ()
insert x startLoc endLoc = do
  unless (isHole x) $ do
    modifyRef' localVarMap $ LVT.insert startLoc endLoc x

get :: App LVT.LocalVarTree
get =
  readRef' localVarMap
