module Scene.Term.Reduce (reduce) where

import Context.App
import Entity.Term qualified as TM

-- import Control.Comonad.Cofree
-- import Data.IntMap qualified as IntMap
-- import Entity.Binder
-- import Entity.DecisionTree qualified as DT
-- import Entity.Ident.Reify qualified as Ident
-- import Entity.LamKind qualified as LK
-- import Entity.Opacity qualified as O
-- import Scene.Term.Subst qualified as Subst

reduce :: TM.Term -> App TM.Term
reduce =
  undefined

--   case term of
--     (m :< TM.Pi xts cod) -> do
--       let (ms, xs, ts) = unzip3 xts
--       ts' <- mapM reduce ts
--       cod' <- reduce cod
--       return (m :< TM.Pi (zip3 ms xs ts') cod')
--     (m :< TM.PiIntro kind xts e) -> do
--       let (ms, xs, ts) = unzip3 xts
--       ts' <- mapM reduce ts
--       e' <- reduce e
--       case kind of
--         LK.Fix (mx, x, t) -> do
--           t' <- reduce t
--           return (m :< TM.PiIntro (LK.Fix (mx, x, t')) (zip3 ms xs ts') e')
--         _ ->
--           return (m :< TM.PiIntro kind (zip3 ms xs ts') e')
--     (m :< TM.PiElim e es) -> do
--       e' <- reduce e
--       es' <- mapM reduce es
--       case e' of
--         (_ :< TM.PiIntro LK.Normal xts (_ :< body))
--           | length xts == length es' -> do
--               reduce $ bind (zip xts es') (m :< body)
--         _ ->
--           return (m :< TM.PiElim e' es')
--     m :< TM.Data attr name es -> do
--       es' <- mapM reduce es
--       return $ m :< TM.Data attr name es'
--     m :< TM.DataIntro attr consName dataArgs consArgs -> do
--       dataArgs' <- mapM reduce dataArgs
--       consArgs' <- mapM reduce consArgs
--       return $ m :< TM.DataIntro attr consName dataArgs' consArgs'
--     m :< TM.DataElim isNoetic oets decisionTree -> do
--       let (os, es, ts) = unzip3 oets
--       es' <- mapM reduce es
--       ts' <- mapM reduce ts
--       decisionTree' <- reduceDecisionTree decisionTree
--       return $ m :< TM.DataElim isNoetic (zip3 os es' ts') decisionTree'
--     m :< TM.Let opacity (mx, x, t) e1 e2 -> do
--       e1' <- reduce e1
--       case opacity of
--         O.Clear
--           | TM.isValue e1' -> do
--               let sub = IntMap.fromList [(Ident.toInt x, Right e1')]
--               Subst.subst sub e2 >>= reduce
--         _ -> do
--           t' <- reduce t
--           e2' <- reduce e2
--           return $ m :< TM.Let opacity (mx, x, t') e1' e2'
--     (m :< TM.Magic magic) -> do
--       magic' <- traverse reduce magic
--       return (m :< TM.Magic magic')
--     _ ->
--       return term

-- reduceDecisionTree ::
--   DT.DecisionTree TM.Term ->
--   App (DT.DecisionTree TM.Term)
-- reduceDecisionTree tree =
--   case tree of
--     DT.Leaf xs e -> do
--       e' <- reduce e
--       return $ DT.Leaf xs e'
--     DT.Unreachable ->
--       return DT.Unreachable
--     DT.Switch (cursorVar, cursor) clauseList -> do
--       cursor' <- reduce cursor
--       clauseList' <- reduceCaseList clauseList
--       return $ DT.Switch (cursorVar, cursor') clauseList'

-- reduceCaseList ::
--   DT.CaseList TM.Term ->
--   App (DT.CaseList TM.Term)
-- reduceCaseList (fallbackTree, clauseList) = do
--   fallbackTree' <- reduceDecisionTree fallbackTree
--   clauseList' <- mapM reduceCase clauseList
--   return (fallbackTree', clauseList')

-- reduceCase ::
--   DT.Case TM.Term ->
--   App (DT.Case TM.Term)
-- reduceCase decisionCase = do
--   let (dataTerms, dataTypes) = unzip $ DT.dataArgs decisionCase
--   dataTerms' <- mapM reduce dataTerms
--   dataTypes' <- mapM reduce dataTypes
--   let (ms, xs, ts) = unzip3 $ DT.consArgs decisionCase
--   ts' <- mapM reduce ts
--   cont' <- reduceDecisionTree $ DT.cont decisionCase
--   return $
--     decisionCase
--       { DT.dataArgs = zip dataTerms' dataTypes',
--         DT.consArgs = zip3 ms xs ts',
--         DT.cont = cont'
--       }

-- bind :: [(BinderF TM.Term, TM.Term)] -> TM.Term -> TM.Term
-- bind binder cont =
--   case binder of
--     [] ->
--       cont
--     ((m, x, t), e1) : rest -> do
--       m :< TM.Let O.Clear (m, x, t) e1 (bind rest cont)
