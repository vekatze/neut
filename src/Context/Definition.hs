module Context.Definition
  ( initialize,
    insert,
    get,
  )
where

import Context.App
import Context.App.Internal
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Entity.Binder
import Entity.DefiniteDescription qualified as DD
import Entity.Opacity qualified as O
import Entity.Term qualified as TM
import Prelude hiding (lookup, read)

initialize :: App ()
initialize = do
  writeRef' defMap Map.empty

insert :: O.Opacity -> DD.DefiniteDescription -> [BinderF TM.Term] -> TM.Term -> App ()
insert opacity name xts e =
  when (opacity == O.Clear) $
    modifyRef' defMap $
      Map.insert name (xts, e)

get :: App (Map.HashMap DD.DefiniteDescription ([BinderF TM.Term], TM.Term))
get =
  readRef' defMap
