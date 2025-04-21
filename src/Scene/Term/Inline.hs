module Scene.Term.Inline (inline) where

import Context.App
import Context.Definition qualified as Definition
import Context.Env qualified as Env
import Context.Throw qualified as Throw
import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class
import Data.Bitraversable (bimapM)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
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
import Scene.Term.Refresh (refresh)
import Scene.Term.Subst qualified as Subst

data Axis = Axis
  { dmap :: Map.HashMap DD.DefiniteDescription ([BinderF TM.Term], TM.Term),
    inlineLimit :: Int,
    currentStepRef :: IORef Int,
    location :: Hint
  }

newAxis :: Hint -> App Axis
newAxis m = do
  source <- Env.getCurrentSource
  dmap <- Definition.get
  let limit = fromMaybe defaultInlineLimit $ moduleInlineLimit (sourceModule source)
  currentStepRef <- liftIO $ newIORef 0
  return
    Axis
      { dmap = dmap,
        inlineLimit = limit,
        currentStepRef = currentStepRef,
        location = m
      }

incrementStep :: Axis -> App ()
incrementStep axis = do
  let Axis {currentStepRef} = axis
  liftIO $ modifyIORef' currentStepRef (+ 1)

detectPossibleInfiniteLoop :: Axis -> App ()
detectPossibleInfiniteLoop axis = do
  let Axis {inlineLimit, currentStepRef, location} = axis
  currentStep <- liftIO $ readIORef currentStepRef
  when (inlineLimit < currentStep) $ do
    Throw.raiseError location $ "Exceeded max recursion depth of " <> T.pack (show inlineLimit)

inline :: Hint -> TM.Term -> App TM.Term
inline m e = do
  axis <- newAxis m
  inline' axis e

inline' :: Axis -> TM.Term -> App TM.Term
inline' axis term = do
  detectPossibleInfiniteLoop axis
  incrementStep axis
  case term of
    m :< TM.Pi impArgs expArgs cod -> do
      impArgs' <- do
        let (ms, xs, ts) = unzip3 impArgs
        ts' <- mapM (inline' axis) ts
        return $ zip3 ms xs ts'
      expArgs' <- do
        let (ms, xs, ts) = unzip3 expArgs
        ts' <- mapM (inline' axis) ts
        return $ zip3 ms xs ts'
      cod' <- inline' axis cod
      return (m :< TM.Pi impArgs' expArgs' cod')
    m :< TM.PiIntro attr@(AttrL.Attr {lamKind}) impArgs expArgs e -> do
      impArgs' <- do
        let (ms, xs, ts) = unzip3 impArgs
        ts' <- mapM (inline' axis) ts
        return $ zip3 ms xs ts'
      expArgs' <- do
        let (ms, xs, ts) = unzip3 expArgs
        ts' <- mapM (inline' axis) ts
        return $ zip3 ms xs ts'
      e' <- inline' axis e
      case lamKind of
        LK.Fix (mx, x, t) -> do
          t' <- inline' axis t
          return (m :< TM.PiIntro (attr {AttrL.lamKind = LK.Fix (mx, x, t')}) impArgs' expArgs' e')
        _ ->
          return (m :< TM.PiIntro attr impArgs' expArgs' e')
    m :< TM.PiElim e es -> do
      e' <- inline' axis e
      es' <- mapM (inline' axis) es
      let Axis {dmap} = axis
      case e' of
        (_ :< TM.PiIntro (AttrL.Attr {lamKind = LK.Normal _}) impArgs expArgs body)
          | xts <- impArgs ++ expArgs,
            length xts == length es' -> do
              if all TM.isValue es'
                then do
                  let (_, xs, _) = unzip3 xts
                  let sub = IntMap.fromList $ zip (map Ident.toInt xs) (map Right es')
                  _ :< body' <- Subst.subst sub body
                  inline' axis $ m :< body'
                else do
                  (xts', _ :< body') <- Subst.subst' IntMap.empty xts body
                  inline' axis $ bind (zip xts' es') (m :< body')
        (_ :< TM.VarGlobal _ dd)
          | Just (xts, body) <- Map.lookup dd dmap -> do
              if all TM.isValue es'
                then do
                  let (_, xs, _) = unzip3 xts
                  let sub = IntMap.fromList $ zip (map Ident.toInt xs) (map Right es')
                  _ :< body' <- Subst.subst sub body
                  body'' <- refresh $ m :< body'
                  inline' axis body''
                else do
                  (xts', _ :< body') <- Subst.subst' IntMap.empty xts body
                  body'' <- refresh $ m :< body'
                  inline' axis $ bind (zip xts' es') body''
        _ ->
          return (m :< TM.PiElim e' es')
    m :< TM.Data attr name es -> do
      es' <- mapM (inline' axis) es
      return $ m :< TM.Data attr name es'
    m :< TM.DataIntro attr consName dataArgs consArgs -> do
      dataArgs' <- mapM (inline' axis) dataArgs
      consArgs' <- mapM (inline' axis) consArgs
      return $ m :< TM.DataIntro attr consName dataArgs' consArgs'
    m :< TM.DataElim isNoetic oets decisionTree -> do
      let (os, es, ts) = unzip3 oets
      es' <- mapM (inline' axis) es
      ts' <- mapM (inline' axis) ts
      let oets' = zip3 os es' ts'
      if isNoetic
        then do
          decisionTree' <- inlineDecisionTree axis decisionTree
          return $ m :< TM.DataElim isNoetic oets' decisionTree'
        else do
          case decisionTree of
            DT.Leaf _ letSeq e -> do
              let sub = IntMap.fromList $ zip (map Ident.toInt os) (map Right es')
              Subst.subst sub (TM.fromLetSeq letSeq e) >>= inline' axis
            DT.Unreachable ->
              return $ m :< TM.DataElim isNoetic oets' DT.Unreachable
            DT.Switch (cursor, _) (fallbackTree, caseList) -> do
              case lookupSplit cursor oets' of
                Just (e@(_ :< TM.DataIntro (AttrDI.Attr {..}) _ _ consArgs), oets'') -> do
                  let (newBaseCursorList, cont) = findClause discriminant fallbackTree caseList
                  let newCursorList = zipWith (\(o, t) arg -> (o, arg, t)) newBaseCursorList consArgs
                  let sub = IntMap.singleton (Ident.toInt cursor) (Right e)
                  dataElim' <- Subst.subst sub $ m :< TM.DataElim isNoetic (oets'' ++ newCursorList) cont
                  inline' axis dataElim'
                _ -> do
                  decisionTree' <- inlineDecisionTree axis decisionTree
                  return $ m :< TM.DataElim isNoetic oets' decisionTree'
    m :< TM.Box t -> do
      t' <- inline' axis t
      return $ m :< TM.Box t'
    m :< TM.BoxIntro letSeq e -> do
      let (xts, es) = unzip letSeq
      xts' <- mapM (inlineBinder axis) xts
      es' <- mapM (inline' axis) es
      e' <- inline' axis e
      return $ m :< TM.BoxIntro (zip xts' es') e'
    m :< TM.Let opacity (mx, x, t) e1 e2 -> do
      e1' <- inline' axis e1
      case opacity of
        O.Clear
          | TM.isValue e1' -> do
              let sub = IntMap.singleton (Ident.toInt x) (Right e1')
              Subst.subst sub e2 >>= inline' axis
        _ -> do
          t' <- inline' axis t
          e2' <- inline' axis e2
          return $ m :< TM.Let opacity (mx, x, t') e1' e2'
    (m :< TM.Magic magic) -> do
      case magic of
        M.Cast _ _ e ->
          inline' axis e
        _ -> do
          magic' <- traverse (inline' axis) magic
          return (m :< TM.Magic magic')
    _ ->
      return term

inlineBinder :: Axis -> BinderF TM.Term -> App (BinderF TM.Term)
inlineBinder axis (m, x, t) = do
  t' <- inline' axis t
  return (m, x, t')

inlineDecisionTree ::
  Axis ->
  DT.DecisionTree TM.Term ->
  App (DT.DecisionTree TM.Term)
inlineDecisionTree axis tree =
  case tree of
    DT.Leaf xs letSeq e -> do
      letSeq' <- mapM (bimapM (inlineBinder axis) (inline' axis)) letSeq
      e' <- inline' axis e
      return $ DT.Leaf xs letSeq' e'
    DT.Unreachable ->
      return DT.Unreachable
    DT.Switch (cursorVar, cursor) clauseList -> do
      cursor' <- inline' axis cursor
      clauseList' <- inlineCaseList axis clauseList
      return $ DT.Switch (cursorVar, cursor') clauseList'

inlineCaseList ::
  Axis ->
  DT.CaseList TM.Term ->
  App (DT.CaseList TM.Term)
inlineCaseList axis (fallbackTree, clauseList) = do
  fallbackTree' <- inlineDecisionTree axis fallbackTree
  clauseList' <- mapM (inlineCase axis) clauseList
  return (fallbackTree', clauseList')

inlineCase ::
  Axis ->
  DT.Case TM.Term ->
  App (DT.Case TM.Term)
inlineCase axis decisionCase = do
  case decisionCase of
    DT.LiteralCase mPat i cont -> do
      cont' <- inlineDecisionTree axis cont
      return $ DT.LiteralCase mPat i cont'
    DT.ConsCase record@(DT.ConsCaseRecord {..}) -> do
      let (dataTerms, dataTypes) = unzip dataArgs
      dataTerms' <- mapM (inline' axis) dataTerms
      dataTypes' <- mapM (inline' axis) dataTypes
      let (ms, xs, ts) = unzip3 consArgs
      ts' <- mapM (inline' axis) ts
      cont' <- inlineDecisionTree axis cont
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
