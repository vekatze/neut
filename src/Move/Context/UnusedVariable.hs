module Move.Context.UnusedVariable
  ( initialize,
    insert,
    delete,
  )
where

import Data.IntMap qualified as IntMap
import Data.Set qualified as S
import Move.Context.App
import Move.Context.App.Internal
import Rule.Hint
import Rule.Ident
import Rule.Ident.Reify
import Rule.VarDefKind
import Prelude hiding (lookup, read)

initialize :: App ()
initialize =
  writeRef' unusedVariableMap IntMap.empty

insert :: Hint -> Ident -> VarDefKind -> App ()
insert m x k =
  modifyRef' unusedVariableMap $ IntMap.insert (toInt x) (m, x, k)

delete :: Ident -> App ()
delete x =
  modifyRef' usedVariableSet $ S.insert (toInt x)
