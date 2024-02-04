module Context.UnusedVariable
  ( initialize,
    insert,
    delete,
    registerRemarks,
  )
where

import Context.App
import Context.App.Internal
import Context.Remark qualified as Remark
import Control.Monad
import Data.IntMap qualified as IntMap
import Data.Set qualified as S
import Entity.Hint
import Entity.Ident
import Entity.Ident.Reify
import Entity.Remark
import Entity.VarDefKind
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

get :: App [(Hint, Ident, VarDefKind)]
get = do
  uenv <- readRef' unusedVariableMap
  set <- readRef' usedVariableSet
  return $ filter (\(_, var, _) -> not (isHole var) && S.notMember (toInt var) set) $ IntMap.elems uenv

registerRemarks :: App ()
registerRemarks = do
  unusedVars <- get
  forM_ unusedVars $ \(mx, x, k) ->
    case k of
      Normal ->
        Remark.insertRemark $ newRemark mx Warning $ "defined but not used: `" <> toText x <> "`"
      Borrowed ->
        Remark.insertRemark $ newRemark mx Warning $ "borrowed but not used: `" <> toText x <> "`"
      Relayed ->
        Remark.insertRemark $ newRemark mx Warning $ "relayed but not used: `" <> toText x <> "`"
