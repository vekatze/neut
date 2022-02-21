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
    locatorAliasMapRef,
    moduleAliasMapRef,
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
import Data.Stmt (QuasiStmt (..), WeakStmt (..))
import qualified Data.Text as T
import Data.WeakTerm
  ( WeakTerm,
    WeakTermF (..),
  )

type NameEnv = Map.HashMap T.Text Ident

discern :: WeakTerm -> IO WeakTerm
discern e = do
  discern' Map.empty e

discernStmtList :: [WeakStmt] -> IO [QuasiStmt]
discernStmtList stmtList =
  case stmtList of
    [] ->
      return []
    WeakStmtDefine isReducible m functionName impArgNum xts codType e : rest -> do
      (xts', nenv) <- discernBinder' Map.empty xts
      codType' <- discern' nenv codType
      e' <- discern' nenv e
      rest' <- discernStmtList rest
      return $ QuasiStmtDefine isReducible m functionName impArgNum xts' codType' e' : rest'
    WeakStmtDefineResource m name discarder copier : rest -> do
      discarder' <- discern discarder
      copier' <- discern copier
      rest' <- discernStmtList rest
      return $ QuasiStmtDefineResource m name discarder' copier' : rest'
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
    _ :< WeakTermTau ->
      return term
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
        moduleAliasMap <- readIORef moduleAliasMapRef
        print moduleAliasMap
        locatorAliasMap <- readIORef locatorAliasMapRef
        print locatorAliasMap
        print topNameSet
        raiseError m $ "undefined constant: " <> x
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
    m :< WeakTermSigma xts -> do
      (xts', _) <- discernBinder nenv xts (m :< WeakTermTau)
      return $ m :< WeakTermSigma xts'
    m :< WeakTermSigmaIntro es -> do
      es' <- mapM (discern' nenv) es
      return $ m :< WeakTermSigmaIntro es'
    m :< WeakTermSigmaElim xts e1 e2 -> do
      e1' <- discern' nenv e1
      (xts', e2') <- discernBinder nenv xts e2
      return $ m :< WeakTermSigmaElim xts' e1' e2'
    _ :< WeakTermConst _ ->
      return term
    _ :< WeakTermAster _ ->
      return term
    m :< WeakTermInt t x -> do
      t' <- discern' nenv t
      return $ m :< WeakTermInt t' x
    m :< WeakTermFloat t x -> do
      t' <- discern' nenv t
      return $ m :< WeakTermFloat t' x
    _ :< WeakTermEnum _ ->
      return term
    _ :< WeakTermEnumIntro _ ->
      return term
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
    m :< WeakTermMagic der -> do
      der' <- traverse (discern' nenv) der
      return $ m :< WeakTermMagic der'
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
    m :< WeakTermArray elemType -> do
      elemType' <- discern' nenv elemType
      return $ m :< WeakTermArray elemType'
    m :< WeakTermArrayIntro elemType elems -> do
      elemType' <- discern' nenv elemType
      elems' <- mapM (discern' nenv) elems
      return $ m :< WeakTermArrayIntro elemType' elems'
    m :< WeakTermArrayAccess subject elemType array index -> do
      subject' <- discern' nenv subject
      elemType' <- discern' nenv elemType
      array' <- discern' nenv array
      index' <- discern' nenv index
      return $ m :< WeakTermArrayAccess subject' elemType' array' index'
    _ :< WeakTermText ->
      return term
    _ :< WeakTermTextIntro _ ->
      return term
    m :< WeakTermCell contentType -> do
      contentType' <- discern' nenv contentType
      return $ m :< WeakTermCell contentType'
    m :< WeakTermCellIntro contentType content -> do
      contentType' <- discern' nenv contentType
      content' <- discern' nenv content
      return $ m :< WeakTermCellIntro contentType' content'
    m :< WeakTermCellRead cell -> do
      cell' <- discern' nenv cell
      return $ m :< WeakTermCellRead cell'
    m :< WeakTermCellWrite cell newValue -> do
      cell' <- discern' nenv cell
      newValue' <- discern' nenv newValue
      return $ m :< WeakTermCellWrite cell' newValue'

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
