module Scene.Term.Inline (inline) where

import Context.App
import Context.Definition qualified as Definition
import Control.Comonad.Cofree
import Data.HashMap.Strict qualified as Map
import Data.IntMap qualified as IntMap
import Entity.DecisionTree qualified as DT
import Entity.Discriminant
import Entity.Ident
import Entity.Ident.Reify qualified as Ident
import Entity.LamKind qualified as LK
import Entity.Magic qualified as M
import Entity.Opacity qualified as O
import Entity.Term qualified as TM
import Scene.Term.Subst qualified as Subst

inline :: TM.Term -> App TM.Term
inline term =
  case term of
    (m :< TM.Pi xts cod) -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM inline ts
      cod' <- inline cod
      return (m :< TM.Pi (zip3 ms xs ts') cod')
    (m :< TM.PiIntro kind xts e) -> do
      let (ms, xs, ts) = unzip3 xts
      ts' <- mapM inline ts
      e' <- inline e
      case kind of
        LK.Fix (mx, x, t) -> do
          t' <- inline t
          return (m :< TM.PiIntro (LK.Fix (mx, x, t')) (zip3 ms xs ts') e')
        _ ->
          return (m :< TM.PiIntro kind (zip3 ms xs ts') e')
    (m :< TM.PiElim e es) -> do
      e' <- inline e
      es' <- mapM inline es
      dmap <- Definition.get
      case e' of
        (_ :< TM.PiIntro (LK.Normal O.Transparent) xts (_ :< body))
          | length xts == length es',
            all TM.isValue es' -> do
              let xs = map (\(_, x, _) -> Ident.toInt x) xts
              let sub = IntMap.fromList $ zip xs (map Right es')
              Subst.subst sub (m :< body) >>= inline
        (_ :< TM.VarGlobal dd _)
          | Just (xts, _ :< body) <- Map.lookup dd dmap,
            all TM.isValue es' -> do
              let xs = map (\(_, x, _) -> Ident.toInt x) xts
              let sub = IntMap.fromList $ zip xs (map Right es')
              Subst.subst sub (m :< body) >>= inline
        _ ->
          return (m :< TM.PiElim e' es')
    m :< TM.Data name consNameList es -> do
      es' <- mapM inline es
      return $ m :< TM.Data name consNameList es'
    m :< TM.DataIntro dataName consName consNameList disc dataArgs consArgs -> do
      dataArgs' <- mapM inline dataArgs
      consArgs' <- mapM inline consArgs
      return $ m :< TM.DataIntro dataName consName consNameList disc dataArgs' consArgs'
    m :< TM.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM inline es
      ts' <- mapM inline ts
      let oets' = zip3 os es' ts'
      if isNoetic
        then do
          decisionTree' <- inlineDecisionTree decisionTree
          return $ m :< TM.DataElim isNoetic oets' decisionTree'
        else do
          case decisionTree of
            DT.Leaf _ e -> do
              let sub = IntMap.fromList $ zip (map Ident.toInt os) (map Right es')
              Subst.subst sub e >>= inline
            DT.Unreachable ->
              return $ m :< TM.DataElim isNoetic oets' DT.Unreachable
            DT.Switch (cursor, _) (fallbackTree, caseList) -> do
              case lookupSplit cursor oets' of
                Just (e@(_ :< TM.DataIntro _ _ _ disc _ consArgs), oets'')
                  | TM.isValue e -> do
                      let (subDom, cont) = findClause disc fallbackTree caseList
                      let sub = IntMap.fromList $ zip (map Ident.toInt subDom) (map Right consArgs)
                      cont' <- Subst.substDecisionTree sub cont
                      inline $ m :< TM.DataElim isNoetic oets'' cont'
                _ -> do
                  decisionTree' <- inlineDecisionTree decisionTree
                  return $ m :< TM.DataElim isNoetic oets' decisionTree'
    m :< TM.Let opacity (mx, x, t) e1 e2 -> do
      e1' <- inline e1
      case opacity of
        O.Transparent
          | TM.isValue e1' -> do
              let sub = IntMap.fromList [(Ident.toInt x, Right e1')]
              Subst.subst sub e2 >>= inline
        _ -> do
          t' <- inline t
          e2' <- inline e2
          return $ m :< TM.Let opacity (mx, x, t') e1' e2'
    (m :< TM.Magic magic) -> do
      case magic of
        M.Cast _ _ e ->
          inline e
        _ -> do
          magic' <- traverse inline magic
          return (m :< TM.Magic magic')
    _ ->
      return term

inlineDecisionTree ::
  DT.DecisionTree TM.Term ->
  App (DT.DecisionTree TM.Term)
inlineDecisionTree tree =
  case tree of
    DT.Leaf xs e -> do
      e' <- inline e
      return $ DT.Leaf xs e'
    DT.Unreachable ->
      return DT.Unreachable
    DT.Switch (cursorVar, cursor) clauseList -> do
      cursor' <- inline cursor
      clauseList' <- inlineCaseList clauseList
      return $ DT.Switch (cursorVar, cursor') clauseList'

inlineCaseList ::
  DT.CaseList TM.Term ->
  App (DT.CaseList TM.Term)
inlineCaseList (fallbackTree, clauseList) = do
  fallbackTree' <- inlineDecisionTree fallbackTree
  clauseList' <- mapM inlineCase clauseList
  return (fallbackTree', clauseList')

inlineCase ::
  DT.Case TM.Term ->
  App (DT.Case TM.Term)
inlineCase (DT.Cons m dd disc dataArgs consArgs tree) = do
  let (dataTerms, dataTypes) = unzip dataArgs
  dataTerms' <- mapM inline dataTerms
  dataTypes' <- mapM inline dataTypes
  let (ms, xs, ts) = unzip3 consArgs
  ts' <- mapM inline ts
  tree' <- inlineDecisionTree tree
  return $ DT.Cons m dd disc (zip dataTerms' dataTypes') (zip3 ms xs ts') tree'

findClause ::
  Discriminant ->
  DT.DecisionTree TM.Term ->
  [DT.Case TM.Term] ->
  ([Ident], DT.DecisionTree TM.Term)
findClause consDisc fallbackTree clauseList =
  case clauseList of
    [] ->
      ([], fallbackTree)
    clause : rest ->
      case findCase consDisc clause of
        Just (consArgs, clauseTree) ->
          (consArgs, clauseTree)
        Nothing ->
          findClause consDisc fallbackTree rest

findCase :: Discriminant -> DT.Case TM.Term -> Maybe ([Ident], DT.DecisionTree TM.Term)
findCase consDisc (DT.Cons _ _ disc _ consArgs tree) =
  if consDisc == disc
    then Just (map (\(_, x, _) -> x) consArgs, tree)
    else Nothing

lookupSplit :: Ident -> [(Ident, b, c)] -> Maybe (b, [(Ident, b, c)])
lookupSplit cursor =
  lookupSplit' cursor []

lookupSplit' :: Ident -> [(Ident, b, c)] -> [(Ident, b, c)] -> Maybe (b, [(Ident, b, c)])
lookupSplit' cursor acc oets =
  case oets of
    [] ->
      Nothing
    oet@(o, e, _) : rest ->
      if o == cursor
        then Just (e, reverse acc ++ rest)
        else lookupSplit' cursor (oet : acc) rest
