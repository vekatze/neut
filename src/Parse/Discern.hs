module Parse.Discern
  ( discernStmtList,
  )
where

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (forM)
import Data.Basic
  ( BinderF,
    EnumCase,
    EnumCaseF (EnumCaseLabel),
    Ident (..),
    LamKindF (LamKindCons, LamKindFix),
    asText,
  )
import Data.Global
  ( enumEnvRef,
    newIdentFromIdent,
    p',
    revEnumEnvRef,
    topNameSetRef,
  )
import qualified Data.HashMap.Lazy as Map
import Data.IORef (readIORef)
import Data.Log (raiseError)
import Data.Namespace
  ( asConstructor,
    asEnum,
    asEnumIntro,
    asEnumLabel,
    asGlobalVar,
    asWeakConstant,
    constructCandList,
    popFromCurrentLocalLocator,
    pushToCurrentLocalLocator,
    resolveSymbol,
    tryCand,
  )
import Data.Stmt (WeakStmt (..))
import qualified Data.Text as T
import Data.WeakTerm
  ( WeakTerm,
    WeakTermF (..),
  )

type NameEnv = Map.HashMap T.Text Ident

discern :: WeakTerm -> IO WeakTerm
discern e = do
  discern' Map.empty e

discernStmtList :: [WeakStmt] -> IO [WeakStmt]
discernStmtList stmtList =
  case stmtList of
    [] ->
      return []
    WeakStmtDefine isReducible m functionName xts codType e : rest -> do
      (xts', nenv) <- discernBinder' Map.empty xts
      codType' <- discern' nenv codType
      e' <- discern' nenv e
      rest' <- discernStmtList rest
      return $ WeakStmtDefine isReducible m functionName xts' codType' e' : rest'
    WeakStmtDefineResource m name discarder copier : rest -> do
      discarder' <- discern discarder
      copier' <- discern copier
      rest' <- discernStmtList rest
      return $ WeakStmtDefineResource m name discarder' copier' : rest'
    WeakStmtSection m sectionName innerStmtList : rest -> do
      pushToCurrentLocalLocator sectionName
      innerStmtList' <- discernStmtList innerStmtList
      _ <- popFromCurrentLocalLocator m
      rest' <- discernStmtList rest
      return $ innerStmtList' ++ rest'

-- Alpha-convert all the variables so that different variables have different names.
discern' :: NameEnv -> WeakTerm -> IO WeakTerm
discern' nenv term =
  case term of
    m :< WeakTermTau ->
      return $ m :< WeakTermTau
    m :< WeakTermVar (I (s, _)) -> do
      case Map.lookup s nenv of
        Just name ->
          return $ m :< WeakTermVar name
        Nothing -> do
          topNameSet <- readIORef topNameSetRef
          candList <- constructCandList s False
          tryCand (resolveSymbol m (asGlobalVar m topNameSet) s candList) $ do
            revEnumEnv <- readIORef revEnumEnvRef
            let candList' = s : candList
            tryCand (resolveSymbol m (asEnumIntro m revEnumEnv) s candList') $ do
              enumEnv <- readIORef enumEnvRef
              tryCand (resolveSymbol m (asEnum m enumEnv) s candList') $
                tryCand (resolveSymbol m (asWeakConstant m) s candList') $
                  raiseError m $ "undefined variable: " <> s
    m :< WeakTermVarGlobal x -> do
      candList <- constructCandList x True
      topNameSet <- readIORef topNameSetRef
      tryCand (resolveSymbol m (asGlobalVar m topNameSet) x candList) $ do
        raiseError m $ "unresolvable definite description: " <> x
    m :< WeakTermPi xts t -> do
      (xts', t') <- discernBinder nenv xts t
      return $ m :< WeakTermPi xts' t'
    m :< WeakTermPiIntro kind xts e -> do
      case kind of
        LamKindFix xt -> do
          (xt' : xts', e') <- discernBinder nenv (xt : xts) e
          return $ m :< WeakTermPiIntro (LamKindFix xt') xts' e'
        LamKindCons dataName consName consNumber dataType -> do
          dataType' <- discern' nenv dataType
          (xts', e') <- discernBinder nenv xts e
          return $ m :< WeakTermPiIntro (LamKindCons dataName consName consNumber dataType') xts' e'
        _ -> do
          (xts', e') <- discernBinder nenv xts e
          return $ m :< WeakTermPiIntro kind xts' e'
    m :< WeakTermPiElim e es -> do
      es' <- mapM (discern' nenv) es
      e' <- discern' nenv e
      return $ m :< WeakTermPiElim e' es'
    m :< WeakTermConst x ->
      return $ m :< WeakTermConst x
    m :< WeakTermAster h ->
      return $ m :< WeakTermAster h
    m :< WeakTermInt t x -> do
      t' <- discern' nenv t
      return $ m :< WeakTermInt t' x
    m :< WeakTermFloat t x -> do
      t' <- discern' nenv t
      return $ m :< WeakTermFloat t' x
    m :< WeakTermEnum s ->
      return $ m :< WeakTermEnum s
    m :< WeakTermEnumIntro x ->
      return $ m :< WeakTermEnumIntro x
    m :< WeakTermEnumElim (e, t) caseList -> do
      e' <- discern' nenv e
      t' <- discern' nenv t
      caseList' <-
        forM caseList $ \(enumCase, body) -> do
          enumCase' <- discernEnumCase enumCase
          body' <- discern' nenv body
          return (enumCase', body')
      return $ m :< WeakTermEnumElim (e', t') caseList'
    m :< WeakTermQuestion e t -> do
      e' <- discern' nenv e
      t' <- discern' nenv t
      return $ m :< WeakTermQuestion e' t'
    m :< WeakTermDerangement der -> do
      der' <- traverse (discern' nenv) der
      return $ m :< WeakTermDerangement der'
    m :< WeakTermMatch mSubject (e, t) clauseList -> do
      mSubject' <- mapM (discern' nenv) mSubject
      e' <- discern' nenv e
      t' <- discern' nenv t
      topNameSet <- readIORef topNameSetRef
      clauseList' <- forM clauseList $ \((mCons, constructorName, xts), body) -> do
        candList <- constructCandList constructorName ("::" `T.isInfixOf` constructorName)
        constructorName' <- resolveSymbol m (asConstructor m topNameSet) constructorName candList
        case constructorName' of
          Just (_, newName) -> do
            (xts', body') <- discernBinder nenv xts body
            return ((mCons, newName, xts'), body')
          Nothing ->
            raiseError m $ "no such constructor is defined: " <> constructorName
      return $ m :< WeakTermMatch mSubject' (e', t') clauseList'
    m :< WeakTermNoema s e -> do
      s' <- discern' nenv s
      e' <- discern' nenv e
      return $ m :< WeakTermNoema s' e'
    m :< WeakTermNoemaIntro (I (x, _)) e ->
      case Map.lookup x nenv of
        Just name -> do
          e' <- discern' nenv e
          return $ m :< WeakTermNoemaIntro name e'
        Nothing ->
          raiseError m $ "undefined subject variable: " <> x
    m :< WeakTermNoemaElim s e -> do
      s' <- newIdentFromIdent s
      e' <- discern' (Map.insert (asText s) s' nenv) e
      return $ m :< WeakTermNoemaElim s' e'

discernBinder ::
  NameEnv ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  IO ([BinderF WeakTerm], WeakTerm)
discernBinder nenv binder e = do
  (binder', nenv') <- discernBinder' nenv binder
  e' <- discern' nenv' e
  return (binder', e')

discernBinder' ::
  NameEnv ->
  [BinderF WeakTerm] ->
  IO ([BinderF WeakTerm], NameEnv)
discernBinder' nenv binder =
  case binder of
    [] -> do
      return ([], nenv)
    (mx, x, t) : xts -> do
      t' <- discern' nenv t
      x' <- newIdentFromIdent x
      (xts', nenv') <- discernBinder' (Map.insert (asText x) x' nenv) xts
      return ((mx, x', t') : xts', nenv')

discernEnumCase :: EnumCase -> IO EnumCase
discernEnumCase enumCase =
  case enumCase of
    m :< EnumCaseLabel l -> do
      revEnumEnv <- readIORef revEnumEnvRef
      candList <- constructCandList l False
      ml <- resolveSymbol m (asEnumLabel m revEnumEnv) l $ l : candList
      case ml of
        Just l' ->
          return l'
        Nothing -> do
          enumEnv <- readIORef enumEnvRef
          p' enumEnv
          raiseError m $ "no such enum-value is defined: " <> l
    _ ->
      return enumCase
