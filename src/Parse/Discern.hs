module Parse.Discern
  ( discern,
    discernStmtList,
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
    resolveSymbol,
    tryCand,
  )
import Data.Stmt (WeakStmt (..))
import qualified Data.Text as T
import Data.WeakTerm
  ( WeakTerm,
    WeakTermF
      ( WeakTermAster,
        WeakTermConst,
        WeakTermDerangement,
        WeakTermEnum,
        WeakTermEnumElim,
        WeakTermEnumIntro,
        WeakTermFloat,
        WeakTermIgnore,
        WeakTermInt,
        WeakTermMatch,
        WeakTermPi,
        WeakTermPiElim,
        WeakTermPiIntro,
        WeakTermQuestion,
        WeakTermTau,
        WeakTermVar,
        WeakTermVarGlobal
      ),
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
    WeakStmtDefine isReducible m x t e : rest -> do
      t' <- discern t
      e' <- discern e
      rest' <- discernStmtList rest
      return $ WeakStmtDefine isReducible m x t' e' : rest'

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
          tryCand (resolveSymbol m (asGlobalVar m topNameSet) s) $ do
            revEnumEnv <- readIORef revEnumEnvRef
            tryCand (resolveSymbol m (asEnumIntro m revEnumEnv) s) $ do
              enumEnv <- readIORef enumEnvRef
              tryCand (resolveSymbol m (asEnum m enumEnv) s) $
                tryCand (resolveSymbol m (asWeakConstant m) s) $
                  raiseError m $ "undefined variable: " <> s
    _ :< WeakTermVarGlobal {} ->
      return term
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
    m :< WeakTermDerangement i es -> do
      es' <- mapM (discern' nenv) es
      return $ m :< WeakTermDerangement i es'
    m :< WeakTermMatch resultType mSubject (e, t) clauseList -> do
      resultType' <- discern' nenv resultType
      mSubject' <- mapM (discern' nenv) mSubject
      e' <- discern' nenv e
      t' <- discern' nenv t
      topNameSet <- readIORef topNameSetRef
      clauseList' <- forM clauseList $ \((mCons, constructorName, xts), body) -> do
        constructorName' <- resolveSymbol m (asConstructor m topNameSet) constructorName
        case constructorName' of
          Just (_, newName) -> do
            (xts', body') <- discernBinder nenv xts body
            return ((mCons, newName, xts'), body')
          Nothing ->
            raiseError m $ "no such constructor is defined: " <> constructorName
      return $ m :< WeakTermMatch resultType' mSubject' (e', t') clauseList'
    m :< WeakTermIgnore e -> do
      e' <- discern' nenv e
      return $ m :< WeakTermIgnore e'

discernBinder ::
  NameEnv ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  IO ([BinderF WeakTerm], WeakTerm)
discernBinder nenv binder e =
  case binder of
    [] -> do
      e' <- discern' nenv e
      return ([], e')
    (mx, x, t) : xts -> do
      t' <- discern' nenv t
      x' <- newIdentFromIdent x
      (xts', e') <- discernBinder (Map.insert (asText x) x' nenv) xts e
      return ((mx, x', t') : xts', e')

discernEnumCase :: EnumCase -> IO EnumCase
discernEnumCase enumCase =
  case enumCase of
    m :< EnumCaseLabel l -> do
      revEnumEnv <- readIORef revEnumEnvRef
      ml <- resolveSymbol m (asEnumLabel m revEnumEnv) l
      case ml of
        Just l' ->
          return l'
        Nothing -> do
          enumEnv <- readIORef enumEnvRef
          p' enumEnv
          raiseError m $ "no such enum-value is defined: " <> l
    _ ->
      return enumCase
