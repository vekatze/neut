module Move.Context.UnusedVariable
  ( initialize,
    insert,
    delete,
    registerRemarks,
  )
where

import Move.Context.App
import Move.Context.App.Internal
import Move.Context.Remark qualified as Remark
import Control.Monad
import Data.IntMap qualified as IntMap
import Data.Set qualified as S
import Rule.Hint
import Rule.Ident
import Rule.Ident.Reify
import Rule.Remark
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
        Remark.insertRemark $ newRemark mx Warning $ "Defined but not used: `" <> toText x <> "`"
      Borrowed ->
        Remark.insertRemark $ newRemark mx Warning $ "Borrowed but not used: `" <> toText x <> "`"
      Relayed ->
        Remark.insertRemark $ newRemark mx Warning $ "Relayed but not used: `" <> toText x <> "`"
