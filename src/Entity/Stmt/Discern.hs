module Entity.Stmt.Discern () where

-- import qualified Context.Locator as Locator
-- import qualified Data.HashMap.Strict as Map
-- import Entity.Stmt
-- import qualified Entity.WeakTerm.Discern as WeakTerm

-- discernStmtList :: WeakTerm.Context -> [PreStmt] -> IO [WeakStmt]
-- discernStmtList ctx stmtList =
--   case stmtList of
--     [] ->
--       return []
--     PreStmtDefine isReducible m functionName impArgNum xts codType e : rest -> do
--       (xts', nenv) <- WeakTerm.discernBinder ctx Map.empty xts
--       codType' <- WeakTerm.discern ctx nenv codType
--       e' <- WeakTerm.discern ctx nenv e
--       rest' <- discernStmtList ctx rest
--       return $ WeakStmtDefine isReducible m functionName impArgNum xts' codType' e' : rest'
--     PreStmtDefineResource m name discarder copier : rest -> do
--       discarder' <- WeakTerm.discern ctx Map.empty discarder
--       copier' <- WeakTerm.discern ctx Map.empty copier
--       rest' <- discernStmtList ctx rest
--       return $ WeakStmtDefineResource m name discarder' copier' : rest'
--     PreStmtSection section innerStmtList : rest -> do
--       Locator.withSection (WeakTerm.locator ctx) section $ do
--         innerStmtList' <- discernStmtList ctx innerStmtList
--         rest' <- discernStmtList ctx rest
--         return $ innerStmtList' ++ rest'
