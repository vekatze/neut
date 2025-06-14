module Language.Term.Move.Inline
  ( Handle,
    new,
    inline,
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Control.Monad.IO.Class
import Data.Bitraversable (bimapM)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.Text qualified as T
import Error.Move.Run (raiseError)
import Error.Rule.EIO (EIO)
import Gensym.Rule.Handle qualified as Gensym
import Language.Common.Rule.Attr.DataIntro qualified as AttrDI
import Language.Common.Rule.Attr.Lam qualified as AttrL
import Language.Common.Rule.Binder
import Language.Common.Rule.DecisionTree qualified as DT
import Language.Common.Rule.DefiniteDescription qualified as DD
import Language.Common.Rule.Discriminant
import Language.Common.Rule.Ident
import Language.Common.Rule.Ident.Reify qualified as Ident
import Language.Common.Rule.LamKind qualified as LK
import Language.Common.Rule.Magic qualified as M
import Language.Common.Rule.Opacity qualified as O
import Language.Term.Move.Refresh qualified as Refresh
import Language.Term.Move.Subst qualified as Subst
import Language.Term.Rule.Prim qualified as P
import Language.Term.Rule.PrimValue qualified as PV
import Language.Term.Rule.Term qualified as TM
import Logger.Rule.Hint

type DefMap =
  Map.HashMap DD.DefiniteDescription ([BinderF TM.Term], TM.Term)

data Handle = Handle
  { substHandle :: Subst.Handle,
    refreshHandle :: Refresh.Handle,
    dmap :: DefMap,
    inlineLimit :: Int,
    currentStepRef :: IORef Int,
    location :: Hint
  }

new :: Gensym.Handle -> DefMap -> Hint -> Int -> IO Handle
new gensymHandle dmap location inlineLimit = do
  let substHandle = Subst.new gensymHandle
  let refreshHandle = Refresh.new gensymHandle
  currentStepRef <- liftIO $ newIORef 0
  return $ Handle {..}

inline :: Handle -> TM.Term -> EIO TM.Term
inline h e = do
  inline' h e

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

inline' :: Handle -> TM.Term -> EIO TM.Term
inline' h term = do
  detectPossibleInfiniteLoop h
  liftIO $ incrementStep h
  case term of
    _ :< TM.Tau {} ->
      return term
    _ :< TM.Var {} ->
      return term
    _ :< TM.VarGlobal {} ->
      return term
    m :< TM.Pi piKind impArgs expArgs cod -> do
      impArgs' <- mapM (\(binder, maybeType) -> do
        binder' <- inlineBinder h binder
        maybeType' <- traverse (inline' h) maybeType
        return (binder', maybeType')) impArgs
      expArgs' <- mapM (inlineBinder h) expArgs
      cod' <- inline' h cod
      return (m :< TM.Pi piKind impArgs' expArgs' cod')
    m :< TM.PiIntro attr@(AttrL.Attr {lamKind}) impArgs expArgs e -> do
      impArgs' <- mapM (inlineBinder h) impArgs
      expArgs' <- mapM (inlineBinder h) expArgs
      e' <- inline' h e
      case lamKind of
        LK.Fix (mx, x, codType) -> do
          codType' <- inline' h codType
          return (m :< TM.PiIntro (attr {AttrL.lamKind = LK.Fix (mx, x, codType')}) impArgs' expArgs' e')
        LK.Normal mName codType -> do
          codType' <- inline' h codType
          return (m :< TM.PiIntro (attr {AttrL.lamKind = LK.Normal mName codType'}) impArgs' expArgs' e')
    m :< TM.PiElim isNoetic e es -> do
      e' <- inline' h e
      es' <- mapM (inline' h) es
      if isNoetic
        then return $ m :< TM.PiElim isNoetic e' es'
        else do
          let Handle {dmap} = h
          case e' of
            (_ :< TM.PiIntro (AttrL.Attr {lamKind = LK.Normal {}}) impArgs expArgs body)
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
                      body'' <- liftIO $ Refresh.refresh (refreshHandle h) $ m :< body'
                      inline' h body''
                    else do
                      (xts', _ :< body') <- liftIO $ Subst.subst' (substHandle h) IntMap.empty xts body
                      body'' <- liftIO $ Refresh.refresh (refreshHandle h) $ m :< body'
                      inline' h $ bind (zip xts' es') body''
            _ ->
              return (m :< TM.PiElim isNoetic e' es')
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
    m :< TM.BoxNoema t -> do
      t' <- inline' h t
      return $ m :< TM.BoxNoema t'
    m :< TM.BoxIntro letSeq e -> do
      letSeq' <- mapM (bimapM (inlineBinder h) (inline' h)) letSeq
      e' <- inline' h e
      return $ m :< TM.BoxIntro letSeq' e'
    m :< TM.BoxElim castSeq mxt e1 uncastSeq e2 -> do
      castSeq' <- mapM (bimapM (inlineBinder h) (inline' h)) castSeq
      e1' <- inline' h e1
      e2' <- inline' h e2
      uncastSeq' <- mapM (bimapM (inlineBinder h) (inline' h)) uncastSeq
      return $ m :< TM.BoxElim castSeq' mxt e1' uncastSeq' e2'
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
    m :< TM.Prim prim -> do
      case prim of
        P.Type _ ->
          return term
        P.Value pv ->
          case pv of
            PV.Int intType size value -> do
              intType' <- inline h intType
              return $ m :< TM.Prim (P.Value (PV.Int intType' size value))
            PV.Float floatType size value -> do
              floatType' <- inline h floatType
              return $ m :< TM.Prim (P.Value (PV.Float floatType' size value))
            PV.Op {} ->
              return term
            PV.StaticText {} ->
              return term
            PV.Rune {} ->
              return term
    (m :< TM.Magic magic) -> do
      case magic of
        M.Cast _ _ e ->
          inline' h e
        _ -> do
          magic' <- traverse (inline' h) magic
          return (m :< TM.Magic magic')
    _ :< TM.Void ->
      return term
    m :< TM.Resource dd resourceID unitType discarder copier -> do
      unitType' <- inline' h unitType
      discarder' <- inline' h discarder
      copier' <- inline' h copier
      return $ m :< TM.Resource dd resourceID unitType' discarder' copier'

inlineBinder :: Handle -> BinderF TM.Term -> EIO (BinderF TM.Term)
inlineBinder h (m, x, t) = do
  t' <- inline' h t
  return (m, x, t')

inlineDecisionTree ::
  Handle ->
  DT.DecisionTree TM.Term ->
  EIO (DT.DecisionTree TM.Term)
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
  EIO (DT.CaseList TM.Term)
inlineCaseList h (fallbackTree, clauseList) = do
  fallbackTree' <- inlineDecisionTree h fallbackTree
  clauseList' <- mapM (inlineCase h) clauseList
  return (fallbackTree', clauseList')

inlineCase ::
  Handle ->
  DT.Case TM.Term ->
  EIO (DT.Case TM.Term)
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
