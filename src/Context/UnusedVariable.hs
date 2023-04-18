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
import Data.Text qualified as T
import Entity.Const
import Entity.Hint
import Entity.Ident
import Entity.Ident.Reify
import Entity.Remark
import Prelude hiding (lookup, read)

initialize :: App ()
initialize =
  writeRef' unusedVariableMap IntMap.empty

insert :: Hint -> Ident -> App ()
insert m x =
  modifyRef' unusedVariableMap $ IntMap.insert (toInt x) (m, x)

delete :: Ident -> App ()
delete x =
  modifyRef' unusedVariableMap $ IntMap.delete (toInt x)

get :: App [(Hint, Ident)]
get = do
  uenv <- readRef' unusedVariableMap
  return $ filter (\(_, var) -> not (isHoleVar var)) $ IntMap.elems uenv

isHoleVar :: Ident -> Bool
isHoleVar (I (varName, _)) =
  holeVarPrefix `T.isPrefixOf` varName

registerRemarks :: App ()
registerRemarks = do
  unusedVars <- get
  forM_ unusedVars $ \(mx, x) ->
    Remark.insertRemark $ newRemark mx Warning $ "defined but not used: `" <> toText' x <> "`"
