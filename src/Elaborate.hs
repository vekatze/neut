module Elaborate
  ( elaborate,
  )
where

import Control.Comonad.Cofree (Cofree (..))
import Control.Concurrent.Async (async)
import Control.Monad (forM, forM_, unless, when)
import Data.Basic
  ( EnumCase,
    EnumCaseF (EnumCaseDefault, EnumCaseLabel),
    Hint,
    LamKind (..),
    Opacity (OpacityTransparent),
    asInt,
  )
import Data.Global
  ( dataEnv,
    enumEnv,
    note,
    promiseEnv,
    substEnv,
    topDefEnv,
    topTypeEnv,
  )
import qualified Data.HashMap.Lazy as Map
import Data.IORef (modifyIORef', readIORef)
import qualified Data.IntMap as IntMap
import Data.List (nub)
import Data.Log (raiseCritical, raiseError)
import Data.LowType
  ( LowType (LowTypeFloat, LowTypeInt),
    asLowTypeMaybe,
  )
import Data.Stmt
  ( HeaderProgram,
    Stmt (..),
    WeakStmt (WeakStmtDef),
    saveCache,
  )
import Data.Term
  ( Binder,
    Pattern,
    Term,
    TermF
      ( TermCase,
        TermConst,
        TermDerangement,
        TermEnum,
        TermEnumElim,
        TermEnumIntro,
        TermFloat,
        TermIgnore,
        TermInt,
        TermPi,
        TermPiElim,
        TermPiIntro,
        TermTau,
        TermVar,
        TermVarGlobal
      ),
    weaken,
  )
import qualified Data.Text as T
import Data.WeakTerm
  ( WeakBinder,
    WeakPattern,
    WeakTerm,
    WeakTermF
      ( WeakTermAster,
        WeakTermCase,
        WeakTermConst,
        WeakTermDerangement,
        WeakTermEnum,
        WeakTermEnumElim,
        WeakTermEnumIntro,
        WeakTermFloat,
        WeakTermIgnore,
        WeakTermInt,
        WeakTermPi,
        WeakTermPiElim,
        WeakTermPiIntro,
        WeakTermQuestion,
        WeakTermTau,
        WeakTermVar,
        WeakTermVarGlobal
      ),
    metaOf,
    toText,
  )
import Elaborate.Infer (infer, inferType, insConstraintEnv)
import Elaborate.Unify (unify)
import Reduce.Term (reduceTerm)
import Reduce.WeakTerm (reduceWeakTerm, substWeakTerm)

elaborate :: ([HeaderProgram], [WeakStmt]) -> IO ([[Stmt]], [Stmt])
elaborate (ss, mainContent) =
  case ss of
    [] -> do
      mainDefList' <- elaborateProgram mainContent
      return ([], mainDefList')
    (srcPath, content, enumInfoList) : rest -> do
      case content of
        Left cache -> do
          forM_ cache $ \stmt -> registerTopLevelDef stmt
          (rest', s') <- elaborate (rest, mainContent)
          return (cache : rest', s')
        Right defList -> do
          headProgram' <- elaborateProgram defList
          promise <- async $ saveCache (srcPath, headProgram') enumInfoList
          modifyIORef' promiseEnv $ \env -> promise : env
          (rest', s') <- elaborate (rest, mainContent)
          return (headProgram' : rest', s')

registerTopLevelDef :: Stmt -> IO ()
registerTopLevelDef (StmtDef isReducible _ x t e) = do
  insTopTypeEnv x $ weaken t
  when isReducible $ do
    modifyIORef' topDefEnv $ \env -> Map.insert x (weaken e) env

elaborateProgram :: [WeakStmt] -> IO [Stmt]
elaborateProgram defList = do
  mapM_ setupDef defList
  defList' <- inferStmtList defList
  -- cs <- readIORef constraintEnv
  -- p "==========================================================="
  -- forM_ cs $ \(e1, e2) -> do
  --   p $ T.unpack $ toText e1
  --   p $ T.unpack $ toText e2
  --   p "---------------------"
  unify
  elaborateStmtList defList'

setupDef :: WeakStmt -> IO ()
setupDef def =
  case def of
    WeakStmtDef isReducible _ x t e -> do
      insTopTypeEnv x t
      when isReducible $
        modifyIORef' topDefEnv $ \env -> Map.insert x e env

inferStmtList :: [WeakStmt] -> IO [WeakStmt]
inferStmtList stmtList =
  case stmtList of
    [] ->
      return []
    WeakStmtDef isReducible m x t e : rest -> do
      (e', te) <- infer e
      t' <- inferType t
      insConstraintEnv te t'
      when (x == "main") $ insConstraintEnv t (m :< WeakTermConst "i64")
      rest' <- inferStmtList rest
      return $ WeakStmtDef isReducible m x t' e' : rest'

elaborateStmtList :: [WeakStmt] -> IO [Stmt]
elaborateStmtList stmtList = do
  case stmtList of
    [] ->
      return []
    WeakStmtDef isReducible m x t e : rest -> do
      e' <- elaborate' e
      t' <- elaborate' t >>= reduceTerm
      insTopTypeEnv x $ weaken t'
      when isReducible $
        modifyIORef' topDefEnv $ \env -> Map.insert x (weaken e') env
      rest' <- elaborateStmtList rest
      return $ StmtDef isReducible m x t' e' : rest'

elaborate' :: WeakTerm -> IO Term
elaborate' term =
  case term of
    m :< WeakTermTau ->
      return $ m :< TermTau
    m :< WeakTermVar x ->
      return $ m :< TermVar x
    m :< WeakTermVarGlobal name ->
      return $ m :< TermVarGlobal name
    m :< WeakTermPi xts t -> do
      xts' <- mapM elaborateWeakBinder xts
      t' <- elaborate' t
      return $ m :< TermPi xts' t'
    m :< WeakTermPiIntro isReducible kind xts e -> do
      kind' <- elaborateKind kind
      xts' <- mapM elaborateWeakBinder xts
      e' <- elaborate' e
      return $ m :< TermPiIntro isReducible kind' xts' e'
    m :< WeakTermPiElim (mh :< WeakTermAster x) es -> do
      sub <- readIORef substEnv
      case IntMap.lookup x sub of
        Nothing ->
          raiseError mh "couldn't instantiate the asterisk here"
        Just (_ :< WeakTermPiIntro OpacityTransparent LamKindNormal xts e)
          | length xts == length es -> do
            let xs = map (\(_, y, _) -> asInt y) xts
            let s = IntMap.fromList $ zip xs es
            substWeakTerm s e >>= elaborate'
        Just e ->
          reduceWeakTerm (m :< WeakTermPiElim e es) >>= elaborate'
    m :< WeakTermPiElim e es -> do
      e' <- elaborate' e
      es' <- mapM elaborate' es
      return $ m :< TermPiElim e' es'
    m :< WeakTermAster _ ->
      raiseCritical m "every meta-variable must be of the form (?M e1 ... en) where n >= 0, but the meta-variable here doesn't fit this pattern"
    m :< WeakTermConst x ->
      return $ m :< TermConst x
    m :< WeakTermInt t x -> do
      t' <- elaborate' t >>= reduceTerm
      case t' of
        _ :< TermConst intTypeStr
          | Just (LowTypeInt size) <- asLowTypeMaybe intTypeStr ->
            return $ m :< TermInt size x
        _ ->
          raiseError m $
            "the term `"
              <> T.pack (show x)
              <> "` is an integer, but its type is: "
              <> toText (weaken t')
    m :< WeakTermFloat t x -> do
      t' <- elaborate' t >>= reduceTerm
      case t' of
        _ :< TermConst floatTypeStr
          | Just (LowTypeFloat size) <- asLowTypeMaybe floatTypeStr ->
            return $ m :< TermFloat size x
        _ ->
          raiseError m $
            "the term `"
              <> T.pack (show x)
              <> "` is a float, but its type is:\n"
              <> toText (weaken t')
    m :< WeakTermEnum k ->
      return $ m :< TermEnum k
    m :< WeakTermEnumIntro x ->
      return $ m :< TermEnumIntro x
    m :< WeakTermEnumElim (e, t) les -> do
      e' <- elaborate' e
      let (ls, es) = unzip les
      es' <- mapM elaborate' es
      t' <- elaborate' t >>= reduceTerm
      case t' of
        _ :< TermEnum x -> do
          checkSwitchExaustiveness m x ls
          checkEnumElim m ls
          -- checkSwitchExaustiveness m x (map snd ls)
          -- checkEnumElim m $ map snd ls
          return $ m :< TermEnumElim (e', t') (zip ls es')
        _ ->
          raiseError m $
            "the type of `"
              <> toText (weaken e')
              <> "` must be an enum type, but is:\n"
              <> toText (weaken t')
    m :< WeakTermQuestion e t -> do
      e' <- elaborate' e
      t' <- elaborate' t
      note m $ toText (weaken t')
      return e'
    m :< WeakTermDerangement i es -> do
      es' <- mapM elaborate' es
      return $ m :< TermDerangement i es'
    m :< WeakTermCase resultType mSubject (e, t) patList -> do
      resultType' <- elaborate' resultType
      mSubject' <- mapM elaborate' mSubject
      e' <- elaborate' e
      t' <- elaborate' t >>= reduceTerm
      denv <- readIORef dataEnv
      case t' of
        _ :< TermPiElim (_ :< TermVarGlobal name) _
          | Just bs <- Map.lookup name denv -> do
            patList' <- elaboratePatternList m bs patList
            return $ m :< TermCase resultType' mSubject' (e', t') patList'
        _ :< TermVarGlobal name
          | Just bs <- Map.lookup name denv -> do
            patList' <- elaboratePatternList m bs patList
            return $ m :< TermCase resultType' mSubject' (e', t') patList'
        _ -> do
          raiseError (metaOf t) $ "the type of this term must be a data-type, but its type is:\n" <> toText (weaken t')
    m :< WeakTermIgnore e -> do
      e' <- elaborate' e
      return $ m :< TermIgnore e'

-- for now
elaboratePatternList :: Hint -> [T.Text] -> [(WeakPattern, WeakTerm)] -> IO [(Pattern, Term)]
elaboratePatternList m bs patList = do
  patList' <- forM patList $ \((mPat, c, xts), body) -> do
    xts' <- mapM elaborateWeakBinder xts
    body' <- elaborate' body
    return ((mPat, c, xts'), body')
  checkCaseSanity m bs patList'
  return patList'

checkCaseSanity :: Hint -> [T.Text] -> [(Pattern, Term)] -> IO ()
checkCaseSanity m bs patList =
  case (bs, patList) of
    ([], []) ->
      return ()
    (b : bsRest, ((mPat, b', _), _) : patListRest) -> do
      if b /= b'
        then raiseError mPat $ "the constructor here is supposed to be `" <> b <> "`, but is: `" <> b' <> "`"
        else checkCaseSanity m bsRest patListRest
    (b : _, []) ->
      raiseError m $ "found a non-exhaustive pattern; the clause for `" <> b <> "` is missing"
    ([], ((mPat, b, _), _) : _) ->
      raiseError mPat $ "found a redundant pattern; this clause for `" <> b <> "` is redundant"

elaborateWeakBinder :: WeakBinder -> IO Binder
elaborateWeakBinder (m, x, t) = do
  t' <- elaborate' t
  return (m, x, t')

elaborateKind :: LamKind WeakBinder -> IO (LamKind Binder)
elaborateKind kind =
  case kind of
    LamKindNormal ->
      return LamKindNormal
    LamKindCons t1 t2 ->
      return $ LamKindCons t1 t2
    LamKindFix xt -> do
      xt' <- elaborateWeakBinder xt
      return $ LamKindFix xt'
    LamKindResourceHandler ->
      return LamKindResourceHandler

checkEnumElim :: Hint -> [EnumCase] -> IO ()
checkEnumElim m ls =
  case ls of
    [] ->
      return ()
    l : rest -> do
      case l of
        _ :< EnumCaseLabel _ ->
          raiseError m "enum-elim"
        _ ->
          checkEnumElim m rest

checkSwitchExaustiveness :: Hint -> T.Text -> [EnumCase] -> IO ()
checkSwitchExaustiveness m x caseList = do
  let containsDefaultCase = doesContainDefaultCase caseList
  enumSet <- lookupEnumSet m x
  let len = toInteger $ length (nub caseList)
  unless (toInteger (length enumSet) <= len || containsDefaultCase) $
    raiseError m "this switch is ill-constructed in that it is not exhaustive"

lookupEnumSet :: Hint -> T.Text -> IO [T.Text]
lookupEnumSet m name = do
  eenv <- readIORef enumEnv
  case Map.lookup name eenv of
    Nothing ->
      raiseError m $ "no such enum defined: " <> name
    Just xis ->
      return $ map fst xis

insTopTypeEnv :: T.Text -> WeakTerm -> IO ()
insTopTypeEnv name t =
  modifyIORef' topTypeEnv $ \env -> Map.insert name t env

doesContainDefaultCase :: [EnumCase] -> Bool
doesContainDefaultCase enumCaseList =
  case enumCaseList of
    [] ->
      False
    (_ :< EnumCaseDefault) : _ ->
      True
    _ : rest ->
      doesContainDefaultCase rest
