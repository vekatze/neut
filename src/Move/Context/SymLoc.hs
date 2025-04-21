module Move.Context.SymLoc
  ( initialize,
    insert,
    get,
  )
where

import Move.Context.App
import Move.Context.App.Internal
import Control.Monad (unless)
import Rule.Hint
import Rule.Ident
import Rule.LocalVarTree qualified as LVT

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
