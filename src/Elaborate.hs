module Elaborate
  ( elaborateMain,
    elaborateOther,
  )
where

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad (forM, forM_, unless, when)
import Data.Basic
  ( BinderF,
    EnumCase,
    EnumCaseF (EnumCaseDefault),
    Hint,
    LamKindF (..),
    PatternF,
    asInt,
    isOpaque,
  )
import Data.Global
  ( dataEnvRef,
    enumEnvRef,
    newIdentFromText,
    note,
    substRef,
    termDefEnvRef,
    termTypeEnvRef,
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
import Data.Source (Source)
import Data.Stmt
  ( EnumInfo,
    Stmt (..),
    WeakStmt (WeakStmtDefine, WeakStmtDefineResource),
    saveCache,
  )
import Data.Term
  ( Term,
    TermF (..),
    weaken,
    weakenBinder,
  )
import qualified Data.Text as T
import Data.WeakTerm
  ( WeakTerm,
    WeakTermF (..),
    metaOf,
    toText,
  )
import Elaborate.Infer (infer, inferBinder, inferType, insConstraintEnv)
import Elaborate.Unify (unify)
import Reduce.Term (reduceTerm)
import Reduce.WeakTerm (reduceWeakTerm, substWeakTerm)

elaborateMain :: T.Text -> Source -> Either [Stmt] ([WeakStmt], [EnumInfo]) -> IO [Stmt]
elaborateMain mainFunctionName =
  elaborate (elaborateProgramMain mainFunctionName)

elaborateOther :: Source -> Either [Stmt] ([WeakStmt], [EnumInfo]) -> IO [Stmt]
elaborateOther =
  elaborate elaborateProgramOther

elaborate :: ([WeakStmt] -> IO [Stmt]) -> Source -> Either [Stmt] ([WeakStmt], [EnumInfo]) -> IO [Stmt]
elaborate defListElaborator source cacheOrStmt =
  case cacheOrStmt of
    Left cache -> do
      forM_ cache registerTopLevelDef
      return cache
    Right (defList, enumInfoList) -> do
      defList' <- defListElaborator defList
      saveCache (source, defList') enumInfoList
      return defList'

registerTopLevelDef :: Stmt -> IO ()
registerTopLevelDef stmt = do
  case stmt of
    StmtDefine opacity m x xts codType e -> do
      insTermTypeEnv x $ weaken $ m :< TermPi xts codType
      unless (isOpaque opacity) $
        modifyIORef' termDefEnvRef $ Map.insert x (opacity, map weakenBinder xts, weaken e)
    StmtDefineResource m name _ _ ->
      insTermTypeEnv name $ m :< WeakTermTau

elaborateProgramMain :: T.Text -> [WeakStmt] -> IO [Stmt]
elaborateProgramMain mainFunctionName = do
  elaborateProgram $ mapM $ inferStmtMain mainFunctionName

elaborateProgramOther :: [WeakStmt] -> IO [Stmt]
elaborateProgramOther = do
  elaborateProgram $ mapM inferStmtOther

elaborateProgram :: ([WeakStmt] -> IO [WeakStmt]) -> [WeakStmt] -> IO [Stmt]
elaborateProgram defListInferrer defList = do
  mapM_ setupDef defList
  defList' <- defListInferrer defList
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
    WeakStmtDefine opacity m f xts codType e -> do
      insTermTypeEnv f $ m :< WeakTermPi xts codType
      modifyIORef' termDefEnvRef $ Map.insert f (opacity, xts, e)
    WeakStmtDefineResource m name _ _ -> do
      insTermTypeEnv name $ m :< WeakTermTau

inferStmtMain :: T.Text -> WeakStmt -> IO WeakStmt
inferStmtMain mainFunctionName stmt = do
  case stmt of
    WeakStmtDefine isReducible m x xts codType e -> do
      (xts', e', codType') <- inferStmt xts e codType
      when (x == mainFunctionName) $
        insConstraintEnv
          (m :< WeakTermPi [] (m :< WeakTermConst "i64"))
          (m :< WeakTermPi xts codType)
      return $ WeakStmtDefine isReducible m x xts' codType' e'
    WeakStmtDefineResource m name discarder copier ->
      inferDefineResource m name discarder copier

inferStmtOther :: WeakStmt -> IO WeakStmt
inferStmtOther stmt = do
  case stmt of
    WeakStmtDefine isReducible m x xts codType e -> do
      (xts', e', codType') <- inferStmt xts e codType
      return $ WeakStmtDefine isReducible m x xts' codType' e'
    WeakStmtDefineResource m name discarder copier ->
      inferDefineResource m name discarder copier

inferDefineResource :: Hint -> T.Text -> WeakTerm -> WeakTerm -> IO WeakStmt
inferDefineResource m name discarder copier = do
  (discarder', td) <- infer discarder
  (copier', tc) <- infer copier
  x <- newIdentFromText "_"
  let botTop = m :< WeakTermPi [(m, x, m :< WeakTermEnum "bottom")] (m :< WeakTermEnum "top")
  let botBot = m :< WeakTermPi [(m, x, m :< WeakTermEnum "bottom")] (m :< WeakTermEnum "bottom")
  insConstraintEnv botTop td
  insConstraintEnv botBot tc
  return $ WeakStmtDefineResource m name discarder' copier'

inferStmt :: [BinderF WeakTerm] -> WeakTerm -> WeakTerm -> IO ([BinderF WeakTerm], WeakTerm, WeakTerm)
inferStmt xts e codType = do
  (xts', (e', te)) <- inferBinder [] xts e
  codType' <- inferType codType
  insConstraintEnv codType' te
  return (xts', e', codType')

elaborateStmtList :: [WeakStmt] -> IO [Stmt]
elaborateStmtList stmtList = do
  case stmtList of
    [] ->
      return []
    WeakStmtDefine opacity m x xts codType e : rest -> do
      e' <- elaborate' e
      xts' <- mapM elaborateWeakBinder xts
      codType' <- elaborate' codType >>= reduceTerm
      insTermTypeEnv x $ weaken $ m :< TermPi xts' codType'
      modifyIORef' termDefEnvRef $ Map.insert x (opacity, map weakenBinder xts', weaken e')
      rest' <- elaborateStmtList rest
      return $ StmtDefine opacity m x xts' codType' e' : rest'
    WeakStmtDefineResource m name discarder copier : rest -> do
      discarder' <- elaborate' discarder
      copier' <- elaborate' copier
      rest' <- elaborateStmtList rest
      return $ StmtDefineResource m name discarder' copier' : rest'

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
    m :< WeakTermPiIntro kind xts e -> do
      kind' <- elaborateKind kind
      xts' <- mapM elaborateWeakBinder xts
      e' <- elaborate' e
      return $ m :< TermPiIntro kind' xts' e'
    m :< WeakTermPiElim (mh :< WeakTermAster x) es -> do
      subst <- readIORef substRef
      case IntMap.lookup x subst of
        Nothing ->
          raiseError mh "couldn't instantiate the asterisk here"
        Just (_ :< WeakTermPiIntro LamKindNormal xts e)
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
        _ -> do
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
    m :< WeakTermDerangement der -> do
      der' <- mapM elaborate' der
      return $ m :< TermDerangement der'
    m :< WeakTermMatch mSubject (e, t) patList -> do
      mSubject' <- mapM elaborate' mSubject
      e' <- elaborate' e
      t' <- elaborate' t >>= reduceTerm
      dataEnv <- readIORef dataEnvRef
      case t' of
        _ :< TermPiElim (_ :< TermVarGlobal name) _
          | Just bs <- Map.lookup name dataEnv -> do
            patList' <- elaboratePatternList m bs patList
            return $ m :< TermMatch mSubject' (e', t') patList'
        _ :< TermVarGlobal name
          | Just bs <- Map.lookup name dataEnv -> do
            patList' <- elaboratePatternList m bs patList
            return $ m :< TermMatch mSubject' (e', t') patList'
        _ -> do
          raiseError (metaOf t) $ "the type of this term must be a data-type, but its type is:\n" <> toText (weaken t')
    m :< WeakTermNoema s e -> do
      s' <- elaborate' s
      e' <- elaborate' e
      return $ m :< TermNoema s' e'
    m :< WeakTermNoemaIntro s e -> do
      e' <- elaborate' e
      return $ m :< TermNoemaIntro s e'
    m :< WeakTermNoemaElim s e -> do
      e' <- elaborate' e
      return $ m :< TermNoemaElim s e'

-- for now
elaboratePatternList :: Hint -> [T.Text] -> [(PatternF WeakTerm, WeakTerm)] -> IO [(PatternF Term, Term)]
elaboratePatternList m bs patList = do
  patList' <- forM patList $ \((mPat, c, xts), body) -> do
    xts' <- mapM elaborateWeakBinder xts
    body' <- elaborate' body
    return ((mPat, c, xts'), body')
  checkCaseSanity m bs patList'
  return patList'

checkCaseSanity :: Hint -> [T.Text] -> [(PatternF Term, Term)] -> IO ()
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

elaborateWeakBinder :: BinderF WeakTerm -> IO (BinderF Term)
elaborateWeakBinder (m, x, t) = do
  t' <- elaborate' t
  return (m, x, t')

elaborateKind :: LamKindF WeakTerm -> IO (LamKindF Term)
elaborateKind kind =
  case kind of
    LamKindNormal ->
      return LamKindNormal
    LamKindCons dataName consName consNumber dataType -> do
      dataType' <- elaborate' dataType
      return $ LamKindCons dataName consName consNumber dataType'
    LamKindFix xt -> do
      xt' <- elaborateWeakBinder xt
      return $ LamKindFix xt'

checkSwitchExaustiveness :: Hint -> T.Text -> [EnumCase] -> IO ()
checkSwitchExaustiveness m x caseList = do
  let containsDefaultCase = doesContainDefaultCase caseList
  enumSet <- lookupEnumSet m x
  let len = toInteger $ length (nub caseList)
  unless (toInteger (length enumSet) <= len || containsDefaultCase) $
    raiseError m "this switch is ill-constructed in that it is not exhaustive"

lookupEnumSet :: Hint -> T.Text -> IO [T.Text]
lookupEnumSet m name = do
  enumEnv <- readIORef enumEnvRef
  case Map.lookup name enumEnv of
    Nothing ->
      raiseError m $ "no such enum defined: " <> name
    Just xis ->
      return $ map fst xis

insTermTypeEnv :: T.Text -> WeakTerm -> IO ()
insTermTypeEnv name t =
  modifyIORef' termTypeEnvRef $ Map.insert name t

doesContainDefaultCase :: [EnumCase] -> Bool
doesContainDefaultCase enumCaseList =
  case enumCaseList of
    [] ->
      False
    (_ :< EnumCaseDefault) : _ ->
      True
    _ : rest ->
      doesContainDefaultCase rest
