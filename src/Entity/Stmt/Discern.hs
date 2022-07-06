module Entity.Stmt.Discern (discernStmtList) where

import qualified Context.Locator as Locator
import Data.Function
import qualified Data.HashMap.Lazy as Map
import Entity.Stmt
import qualified Entity.WeakTerm.Discern as WeakTerm

discernStmtList :: WeakTerm.Context -> [WeakStmt] -> IO [QuasiStmt]
discernStmtList ctx stmtList =
  case stmtList of
    [] ->
      return []
    WeakStmtDefine isReducible m functionName impArgNum xts codType e : rest -> do
      (xts', nenv) <- WeakTerm.discernBinder ctx Map.empty xts
      codType' <- WeakTerm.discern ctx nenv codType
      e' <- WeakTerm.discern ctx nenv e
      rest' <- discernStmtList ctx rest
      return $ QuasiStmtDefine isReducible m functionName impArgNum xts' codType' e' : rest'
    WeakStmtDefineResource m name discarder copier : rest -> do
      discarder' <- WeakTerm.discern ctx Map.empty discarder
      copier' <- WeakTerm.discern ctx Map.empty copier
      rest' <- discernStmtList ctx rest
      return $ QuasiStmtDefineResource m name discarder' copier' : rest'
    WeakStmtSection m sectionName innerStmtList : rest -> do
      Locator.pushToCurrentLocalLocator (WeakTerm.locator ctx) sectionName
      innerStmtList' <- discernStmtList ctx innerStmtList
      _ <- Locator.popFromCurrentLocalLocator (ctx & WeakTerm.locator) m
      rest' <- discernStmtList ctx rest
      return $ innerStmtList' ++ rest'

-- discern :: WeakStmt -> IO QuasiStmt
-- discern stmt =
--   case stmt of
--     WeakStmtDefine isReducible m functionName impArgNum xts codType e -> do
--       (xts', nenv) <- WeakTerm.discernBinder Map.empty xts
--       codType' <- WeakTerm.discern nenv codType
--       e' <- WeakTerm.discern nenv e
--       return $ QuasiStmtDefine isReducible m functionName impArgNum xts' codType' e'
--     WeakStmtDefineResource m name discarder copier -> do
--       discarder' <- WeakTerm.discern Map.empty discarder
--       copier' <- WeakTerm.discern Map.empty copier
--       return $ QuasiStmtDefineResource m name discarder' copier'
--     WeakStmtSection m sectionName innerStmtList -> do
--       pushToCurrentLocalLocator sectionName
--       innerStmtList' <- mapM discern innerStmtList
--       _ <- popFromCurrentLocalLocator m
--       return $ innerStmtList' ++ rest'
