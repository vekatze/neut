module Move.Scene.Term.Inline (inline) where

import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class
import Data.Bitraversable (bimapM)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Move.Context.App
import Move.Context.Definition qualified as Definition
import Move.Context.EIO (EIO, raiseError, toApp)
import Move.Context.Env qualified as Env
import Move.Scene.Term.Refresh qualified as Refresh
import Move.Scene.Term.Subst qualified as Subst
import Rule.Attr.DataIntro qualified as AttrDI
import Rule.Attr.Lam qualified as AttrL
import Rule.Binder
import Rule.Const (defaultInlineLimit)
import Rule.DecisionTree qualified as DT
import Rule.DefiniteDescription qualified as DD
import Rule.Discriminant
import Rule.Hint
import Rule.Ident
import Rule.Ident.Reify qualified as Ident
import Rule.LamKind qualified as LK
import Rule.Magic qualified as M
import Rule.Module (moduleInlineLimit)
import Rule.Opacity qualified as O
import Rule.Source (sourceModule)
import Rule.Term qualified as TM

data Handle = Handle
  { substHandle :: Subst.Handle,
    refreshHandle :: Refresh.Handle,
    dmap :: Map.HashMap DD.DefiniteDescription ([BinderF TM.Term], TM.Term),
    inlineLimit :: Int,
    currentStepRef :: IORef Int,
    location :: Hint
  }

new :: Hint -> App Handle
new location = do
  substHandle <- Subst.new
  refreshHandle <- Refresh.new
  source <- Env.getCurrentSource
  dmap <- Definition.get
  let inlineLimit = fromMaybe defaultInlineLimit $ moduleInlineLimit (sourceModule source)
  currentStepRef <- liftIO $ newIORef 0
  return $ Handle {..}

incrementStep :: Handle -> IO ()
incrementStep h = do
  let Handle {currentStepRef} = h
  modifyIORef' currentStepRef (+ 1)

detectPossibleInfiniteLoop :: Handle -> EIO ()
detectPossibleInfiniteLoop h = do
  let Handle {inlineLimit, currentStepRef, location} = h
  currentStep <- liftIO $ readIORef currentStepRef
  when (inlineLimit < currentStep) $ do
    raiseError location $ "Exceeded max recursion depth of " <> T.pack (show inlineLimit)

inline :: Hint -> TM.Term -> App TM.Term
inline m e = do
  h <- new m
  inline' h e

inline' :: Handle -> TM.Term -> App TM.Term
inline' h term = do
  toApp $ detectPossibleInfiniteLoop h
  liftIO $ incrementStep h
  case term of
    m :< TM.Pi impArgs expArgs cod -> do
      impArgs' <- do
        let (ms, xs, ts) = unzip3 impArgs
        ts' <- mapM (inline' h) ts
        return $ zip3 ms xs ts'
      expArgs' <- do
        let (ms, xs, ts) = unzip3 expArgs
        ts' <- mapM (inline' h) ts
        return $ zip3 ms xs ts'
      cod' <- inline' h cod
      return (m :< TM.Pi impArgs' expArgs' cod')
    m :< TM.PiIntro attr@(AttrL.Attr {lamKind}) impArgs expArgs e -> do
      impArgs' <- do
        let (ms, xs, ts) = unzip3 impArgs
        ts' <- mapM (inline' h) ts
        return $ zip3 ms xs ts'
      expArgs' <- do
        let (ms, xs, ts) = unzip3 expArgs
        ts' <- mapM (inline' h) ts
        return $ zip3 ms xs ts'
      e' <- inline' h e
      case lamKind of
        LK.Fix (mx, x, t) -> do
          t' <- inline' h t
          return (m :< TM.PiIntro (attr {AttrL.lamKind = LK.Fix (mx, x, t')}) impArgs' expArgs' e')
        _ ->
          return (m :< TM.PiIntro attr impArgs' expArgs' e')
    m :< TM.PiElim e es -> do
      e' <- inline' h e
      es' <- mapM (inline' h) es
      let Handle {dmap} = h
      case e' of
        (_ :< TM.PiIntro (AttrL.Attr {lamKind = LK.Normal _}) impArgs expArgs body)
          | xts <- impArgs ++ expArgs,
            length xts == length es' -> do
              if all TM.isValue es'
                then do
                  let (_, xs, _) = unzip3 xts
                  let sub = IntMap.fromList $ zip (map Ident.toInt xs) (map Right es')
                  _ :< body' <- liftIO $ Subst.subst (substHandle h) sub body
                  inline' h $ m :< body'
                else do
                  (xts', _ :< body') <- liftIO $ Subst.subst' (substHandle h) IntMap.empty xts body
                  inline' h $ bind (zip xts' es') (m :< body')
        (_ :< TM.VarGlobal _ dd)
          | Just (xts, body) <- Map.lookup dd dmap -> do
              if all TM.isValue es'
                then do
                  let (_, xs, _) = unzip3 xts
                  let sub = IntMap.fromList $ zip (map Ident.toInt xs) (map Right es')
                  _ :< body' <- liftIO $ Subst.subst (substHandle h) sub body
                  body'' <- Refresh.refresh (refreshHandle h) $ m :< body'
                  inline' h body''
                else do
                  (xts', _ :< body') <- liftIO $ Subst.subst' (substHandle h) IntMap.empty xts body
                  body'' <- Refresh.refresh (refreshHandle h) $ m :< body'
                  inline' h $ bind (zip xts' es') body''
        _ ->
          return (m :< TM.PiElim e' es')
    m :< TM.Data attr name es -> do
      es' <- mapM (inline' h) es
      return $ m :< TM.Data attr name es'
    m :< TM.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (inline' h) dataArgs
      consArgs' <- mapM (inline' h) consArgs
      return $ m :< TM.DataIntro attr consName dataArgs' consArgs'
    m :< TM.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (inline' h) es
      ts' <- mapM (inline' h) ts
      let oets' = zip3 os es' ts'
      if isNoetic
        then do
          decisionTree' <- inlineDecisionTree h decisionTree
          return $ m :< TM.DataElim isNoetic oets' decisionTree'
        else do
          case decisionTree of
            DT.Leaf _ letSeq e -> do
              let sub = IntMap.fromList $ zip (map Ident.toInt os) (map Right es')
              liftIO (Subst.subst (substHandle h) sub (TM.fromLetSeq letSeq e)) >>= inline' h
            DT.Unreachable ->
              return $ m :< TM.DataElim isNoetic oets' DT.Unreachable
            DT.Switch (cursor, _) (fallbackTree, caseList) -> do
              case lookupSplit cursor oets' of
                Just (e@(_ :< TM.DataIntro (AttrDI.Attr {..}) _ _ consArgs), oets'') -> do
                  let (newBaseCursorList, cont) = findClause discriminant fallbackTree caseList
                  let newCursorList = zipWith (\(o, t) arg -> (o, arg, t)) newBaseCursorList consArgs
                  let sub = IntMap.singleton (Ident.toInt cursor) (Right e)
                  dataElim' <- liftIO $ Subst.subst (substHandle h) sub $ m :< TM.DataElim isNoetic (oets'' ++ newCursorList) cont
                  inline' h dataElim'
                _ -> do
                  decisionTree' <- inlineDecisionTree h decisionTree
                  return $ m :< TM.DataElim isNoetic oets' decisionTree'
    m :< TM.Box t -> do
      t' <- inline' h t
      return $ m :< TM.Box t'
    m :< TM.BoxIntro letSeq e -> do
      let (xts, es) = unzip letSeq
      xts' <- mapM (inlineBinder h) xts
      es' <- mapM (inline' h) es
      e' <- inline' h e
      return $ m :< TM.BoxIntro (zip xts' es') e'
    m :< TM.Let opacity (mx, x, t) e1 e2 -> do
      e1' <- inline' h e1
      case opacity of
        O.Clear
          | TM.isValue e1' -> do
              let sub = IntMap.singleton (Ident.toInt x) (Right e1')
              liftIO (Subst.subst (substHandle h) sub e2) >>= inline' h
        _ -> do
          t' <- inline' h t
          e2' <- inline' h e2
          return $ m :< TM.Let opacity (mx, x, t') e1' e2'
    (m :< TM.Magic magic) -> do
      case magic of
        M.Cast _ _ e ->
          inline' h e
        _ -> do
          magic' <- traverse (inline' h) magic
          return (m :< TM.Magic magic')
    _ ->
      return term

inlineBinder :: Handle -> BinderF TM.Term -> App (BinderF TM.Term)
inlineBinder h (m, x, t) = do
  t' <- inline' h t
  return (m, x, t')

inlineDecisionTree ::
  Handle ->
  DT.DecisionTree TM.Term ->
  App (DT.DecisionTree TM.Term)
inlineDecisionTree h tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      letSeq' <- mapM (bimapM (inlineBinder h) (inline' h)) letSeq
      e' <- inline' h e
      return $ DT.Leaf xs letSeq' e'
    DT.Unreachable ->
      return DT.Unreachable
    DT.Switch (cursorVar, cursor) clauseList -> do
      cursor' <- inline' h cursor
      clauseList' <- inlineCaseList h clauseList
      return $ DT.Switch (cursorVar, cursor') clauseList'

inlineCaseList ::
  Handle ->
  DT.CaseList TM.Term ->
  App (DT.CaseList TM.Term)
inlineCaseList h (fallbackTree, clauseList) = do
  fallbackTree' <- inlineDecisionTree h fallbackTree
  clauseList' <- mapM (inlineCase h) clauseList
  return (fallbackTree', clauseList')

inlineCase ::
  Handle ->
  DT.Case TM.Term ->
  App (DT.Case TM.Term)
inlineCase h decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- inlineDecisionTree h cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM (inline' h) dataTerms
      dataTypes' <- mapM (inline' h) dataTypes
      let (ms, xs, ts) = unzip3 consArgs
      ts' <- mapM (inline' h) ts
      cont' <- inlineDecisionTree h cont
      return $
        DT.ConsCase
          record
            { DT.dataArgs = zip dataTerms' dataTypes',
              DT.consArgs = zip3 ms xs ts',
              DT.cont = cont'
            }

findClause ::
  Discriminant ->
  DT.DecisionTree TM.Term ->
  [DT.Case TM.Term] ->
  ([(Ident, TM.Term)], DT.DecisionTree TM.Term)
findClause consDisc fallbackTree clauseList =
  case clauseList of
    [] ->
      ([], fallbackTree)
    clause : rest ->
      case DT.findCase consDisc clause of
        Just (consArgs, clauseTree) ->
          (consArgs, clauseTree)
        Nothing ->
          findClause consDisc fallbackTree rest

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

bind :: [(BinderF TM.Term, TM.Term)] -> TM.Term -> TM.Term
bind binder cont =
  case binder of
    [] ->
      cont
    ((m, x, t), e1) : rest -> do
      m :< TM.Let O.Clear (m, x, t) e1 (bind rest cont)
