module Scene.Elaborate
  ( elaborateMain,
    elaborateOther,
  )
where

import Context.Log
import Control.Comonad.Cofree
import Control.Monad
import qualified Data.HashMap.Lazy as Map
import Data.IORef
import qualified Data.IntMap as IntMap
import Data.List
import qualified Data.Text as T
import Entity.Binder
import Entity.EnumCase
import Entity.EnumInfo
import Entity.Global
import Entity.Hint
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import Entity.Log
import Entity.Opacity
import Entity.Pattern
import Entity.PrimNum
import qualified Entity.PrimNum.FromText as PrimNum
import Entity.Source
import Entity.Stmt
import Entity.Term
import qualified Entity.Term.Reduce as Term
import Entity.Term.Weaken
import Entity.WeakTerm
import qualified Entity.WeakTerm.Reduce as WeakTerm
import qualified Entity.WeakTerm.Subst as WeakTerm
import Entity.WeakTerm.ToText
import Scene.Elaborate.Infer
import Scene.Elaborate.Unify

elaborateMain :: LogContext IO -> T.Text -> Source -> Either [Stmt] ([QuasiStmt], [EnumInfo]) -> IO [Stmt]
elaborateMain logCtx mainFunctionName =
  elaborate (elaborateProgramMain logCtx mainFunctionName)

elaborateOther :: LogContext IO -> Source -> Either [Stmt] ([QuasiStmt], [EnumInfo]) -> IO [Stmt]
elaborateOther logCtx =
  elaborate (elaborateProgramOther logCtx)

elaborate :: ([QuasiStmt] -> IO [Stmt]) -> Source -> Either [Stmt] ([QuasiStmt], [EnumInfo]) -> IO [Stmt]
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
    StmtDefine opacity m x impArgNum xts codType e -> do
      modifyIORef' impArgEnvRef $ Map.insert x impArgNum
      insTermTypeEnv x $ weaken $ m :< TermPi xts codType
      unless (isOpaque opacity) $
        modifyIORef' termDefEnvRef $ Map.insert x (opacity, map weakenBinder xts, weaken e)
    StmtDefineResource m name _ _ ->
      insTermTypeEnv name $ m :< WeakTermTau

elaborateProgramMain :: LogContext IO -> T.Text -> [QuasiStmt] -> IO [Stmt]
elaborateProgramMain logCtx mainFunctionName = do
  elaborateProgram logCtx $ mapM $ inferStmtMain mainFunctionName

elaborateProgramOther :: LogContext IO -> [QuasiStmt] -> IO [Stmt]
elaborateProgramOther logCtx = do
  elaborateProgram logCtx $ mapM inferStmtOther

elaborateProgram :: LogContext IO -> ([QuasiStmt] -> IO [QuasiStmt]) -> [QuasiStmt] -> IO [Stmt]
elaborateProgram logCtx defListInferrer defList = do
  defList' <- mapM setupDef defList >>= defListInferrer . concat
  -- cs <- readIORef constraintEnv
  -- p "==========================================================="
  -- forM_ cs $ \(e1, e2) -> do
  --   p $ T.unpack $ toText e1
  --   p $ T.unpack $ toText e2
  --   p "---------------------"
  unify
  elaborateStmtList logCtx defList'

setupDef :: QuasiStmt -> IO [QuasiStmt]
setupDef def =
  case def of
    QuasiStmtDefine opacity m f impArgNum xts codType e -> do
      (xts', codType') <- arrangeBinder [] xts codType
      insTermTypeEnv f $ m :< WeakTermPi xts' codType'
      modifyIORef' impArgEnvRef $ Map.insert f impArgNum
      modifyIORef' termDefEnvRef $ Map.insert f (opacity, xts', e)
      return [QuasiStmtDefine opacity m f impArgNum xts' codType' e]
    QuasiStmtDefineResource m name discarder copier -> do
      insTermTypeEnv name $ m :< WeakTermTau
      return [QuasiStmtDefineResource m name discarder copier]

inferStmtMain :: T.Text -> QuasiStmt -> IO QuasiStmt
inferStmtMain mainFunctionName stmt = do
  case stmt of
    QuasiStmtDefine isReducible m x impArgNum xts codType e -> do
      (xts', e', codType') <- inferStmt xts e codType
      when (x == mainFunctionName) $
        insConstraintEnv
          (m :< WeakTermPi [] (m :< WeakTermConst "i64"))
          (m :< WeakTermPi xts codType)
      return $ QuasiStmtDefine isReducible m x impArgNum xts' codType' e'
    QuasiStmtDefineResource m name discarder copier ->
      inferDefineResource m name discarder copier

inferStmtOther :: QuasiStmt -> IO QuasiStmt
inferStmtOther stmt = do
  case stmt of
    QuasiStmtDefine isReducible m x impArgNum xts codType e -> do
      (xts', e', codType') <- inferStmt xts e codType
      return $ QuasiStmtDefine isReducible m x impArgNum xts' codType' e'
    QuasiStmtDefineResource m name discarder copier ->
      inferDefineResource m name discarder copier

inferDefineResource :: Hint -> T.Text -> WeakTerm -> WeakTerm -> IO QuasiStmt
inferDefineResource m name discarder copier = do
  (discarder', td) <- infer discarder
  (copier', tc) <- infer copier
  x <- newIdentFromText "_"
  let botTop = m :< WeakTermPi [(m, x, m :< WeakTermEnum "bottom")] (m :< WeakTermEnum "top")
  let botBot = m :< WeakTermPi [(m, x, m :< WeakTermEnum "bottom")] (m :< WeakTermEnum "bottom")
  insConstraintEnv botTop td
  insConstraintEnv botBot tc
  return $ QuasiStmtDefineResource m name discarder' copier'

inferStmt :: [BinderF WeakTerm] -> WeakTerm -> WeakTerm -> IO ([BinderF WeakTerm], WeakTerm, WeakTerm)
inferStmt xts e codType = do
  (xts', (e', te)) <- inferBinder [] xts e
  codType' <- inferType codType
  insConstraintEnv codType' te
  return (xts', e', codType')

elaborateStmtList :: LogContext IO -> [QuasiStmt] -> IO [Stmt]
elaborateStmtList logCtx stmtList = do
  case stmtList of
    [] ->
      return []
    QuasiStmtDefine opacity m x impArgNum xts codType e : rest -> do
      e' <- elaborate' logCtx e
      xts' <- mapM (elaborateWeakBinder logCtx) xts
      codType' <- elaborate' logCtx codType >>= Term.reduce
      insTermTypeEnv x $ weaken $ m :< TermPi xts' codType'
      modifyIORef' termDefEnvRef $ Map.insert x (opacity, map weakenBinder xts', weaken e')
      rest' <- elaborateStmtList logCtx rest
      return $ StmtDefine opacity m x impArgNum xts' codType' e' : rest'
    QuasiStmtDefineResource m name discarder copier : rest -> do
      discarder' <- elaborate' logCtx discarder
      copier' <- elaborate' logCtx copier
      rest' <- elaborateStmtList logCtx rest
      return $ StmtDefineResource m name discarder' copier' : rest'

elaborate' :: LogContext IO -> WeakTerm -> IO Term
elaborate' logCtx term =
  case term of
    m :< WeakTermTau ->
      return $ m :< TermTau
    m :< WeakTermVar x ->
      return $ m :< TermVar x
    m :< WeakTermVarGlobal name ->
      return $ m :< TermVarGlobal name
    m :< WeakTermPi xts t -> do
      xts' <- mapM (elaborateWeakBinder logCtx) xts
      t' <- elaborate' logCtx t
      return $ m :< TermPi xts' t'
    m :< WeakTermPiIntro kind xts e -> do
      kind' <- elaborateKind logCtx kind
      xts' <- mapM (elaborateWeakBinder logCtx) xts
      e' <- elaborate' logCtx e
      return $ m :< TermPiIntro kind' xts' e'
    m :< WeakTermPiElim (mh :< WeakTermAster x) es -> do
      subst <- readIORef substRef
      case IntMap.lookup x subst of
        Nothing ->
          raiseError mh "couldn't instantiate the asterisk here"
        Just (_ :< WeakTermPiIntro LamKindNormal xts e)
          | length xts == length es -> do
            let xs = map (\(_, y, _) -> Ident.toInt y) xts
            let s = IntMap.fromList $ zip xs es
            WeakTerm.subst s e >>= elaborate' logCtx
        Just e ->
          WeakTerm.reduce (m :< WeakTermPiElim e es) >>= elaborate' logCtx
    m :< WeakTermPiElim e es -> do
      e' <- elaborate' logCtx e
      es' <- mapM (elaborate' logCtx) es
      return $ m :< TermPiElim e' es'
    m :< WeakTermSigma xts -> do
      xts' <- mapM (elaborateWeakBinder logCtx) xts
      return $ m :< TermSigma xts'
    m :< WeakTermSigmaIntro es -> do
      es' <- mapM (elaborate' logCtx) es
      return $ m :< TermSigmaIntro es'
    m :< WeakTermSigmaElim xts e1 e2 -> do
      e1' <- elaborate' logCtx e1
      xts' <- mapM (elaborateWeakBinder logCtx) xts
      e2' <- elaborate' logCtx e2
      return $ m :< TermSigmaElim xts' e1' e2'
    m :< WeakTermLet mxt e1 e2 -> do
      e1' <- elaborate' logCtx e1
      mxt' <- elaborateWeakBinder logCtx mxt
      e2' <- elaborate' logCtx e2
      return $ m :< TermLet mxt' e1' e2'
    m :< WeakTermAster _ ->
      raiseCritical m "every meta-variable must be of the form (?M e1 ... en) where n >= 0, but the meta-variable here doesn't fit this pattern"
    m :< WeakTermConst x ->
      return $ m :< TermConst x
    m :< WeakTermInt t x -> do
      t' <- elaborate' logCtx t >>= Term.reduce
      case t' of
        _ :< TermConst intTypeStr
          | Just (PrimNumInt size) <- PrimNum.fromText intTypeStr ->
            return $ m :< TermInt size x
        _ -> do
          raiseError m $
            "the term `"
              <> T.pack (show x)
              <> "` is an integer, but its type is: "
              <> toText (weaken t')
    m :< WeakTermFloat t x -> do
      t' <- elaborate' logCtx t >>= Term.reduce
      case t' of
        _ :< TermConst floatTypeStr
          | Just (PrimNumFloat size) <- PrimNum.fromText floatTypeStr ->
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
      e' <- elaborate' logCtx e
      let (ls, es) = unzip les
      es' <- mapM (elaborate' logCtx) es
      t' <- elaborate' logCtx t >>= Term.reduce
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
      e' <- elaborate' logCtx e
      t' <- elaborate' logCtx t
      printNote logCtx m $ toText (weaken t')
      return e'
    m :< WeakTermMagic der -> do
      der' <- mapM (elaborate' logCtx) der
      return $ m :< TermMagic der'
    m :< WeakTermMatch mSubject (e, t) patList -> do
      mSubject' <- mapM (elaborate' logCtx) mSubject
      e' <- elaborate' logCtx e
      t' <- elaborate' logCtx t >>= Term.reduce
      dataEnv <- readIORef dataEnvRef
      case t' of
        _ :< TermPiElim (_ :< TermVarGlobal name) _
          | Just bs <- Map.lookup name dataEnv -> do
            patList' <- elaboratePatternList logCtx m bs patList
            return $ m :< TermMatch mSubject' (e', t') patList'
        _ :< TermVarGlobal name
          | Just bs <- Map.lookup name dataEnv -> do
            patList' <- elaboratePatternList logCtx m bs patList
            return $ m :< TermMatch mSubject' (e', t') patList'
        _ -> do
          raiseError (metaOf t) $ "the type of this term must be a data-type, but its type is:\n" <> toText (weaken t')
    m :< WeakTermNoema s e -> do
      s' <- elaborate' logCtx s
      e' <- elaborate' logCtx e
      return $ m :< TermNoema s' e'
    m :< WeakTermNoemaIntro s e -> do
      e' <- elaborate' logCtx e
      return $ m :< TermNoemaIntro s e'
    m :< WeakTermNoemaElim s e -> do
      e' <- elaborate' logCtx e
      return $ m :< TermNoemaElim s e'
    m :< WeakTermArray elemType -> do
      elemType' <- elaborate' logCtx elemType
      case elemType' of
        _ :< TermConst typeStr
          | Just (PrimNumInt size) <- PrimNum.fromText typeStr ->
            return $ m :< TermArray (PrimNumInt size)
          | Just (PrimNumFloat size) <- PrimNum.fromText typeStr ->
            return $ m :< TermArray (PrimNumFloat size)
        _ ->
          raiseError m $ "invalid element type:\n" <> toText (weaken elemType')
    m :< WeakTermArrayIntro elemType elems -> do
      elemType' <- elaborate' logCtx elemType
      elems' <- mapM (elaborate' logCtx) elems
      case elemType' of
        _ :< TermConst typeStr
          | Just (PrimNumInt size) <- PrimNum.fromText typeStr ->
            return $ m :< TermArrayIntro (PrimNumInt size) elems'
          | Just (PrimNumFloat size) <- PrimNum.fromText typeStr ->
            return $ m :< TermArrayIntro (PrimNumFloat size) elems'
        _ ->
          raiseError m $ "invalid element type:\n" <> toText (weaken elemType')
    m :< WeakTermArrayAccess subject elemType array index -> do
      subject' <- elaborate' logCtx subject
      elemType' <- elaborate' logCtx elemType
      array' <- elaborate' logCtx array
      index' <- elaborate' logCtx index
      case elemType' of
        _ :< TermConst typeStr
          | Just (PrimNumInt size) <- PrimNum.fromText typeStr ->
            return $ m :< TermArrayAccess subject' (PrimNumInt size) array' index'
          | Just (PrimNumFloat size) <- PrimNum.fromText typeStr ->
            return $ m :< TermArrayAccess subject' (PrimNumFloat size) array' index'
        _ ->
          raiseError m $ "invalid element type:\n" <> toText (weaken elemType')
    m :< WeakTermText ->
      return $ m :< TermText
    m :< WeakTermTextIntro text ->
      return $ m :< TermTextIntro text
    m :< WeakTermCell contentType -> do
      contentType' <- elaborate' logCtx contentType
      return $ m :< TermCell contentType'
    m :< WeakTermCellIntro contentType content -> do
      contentType' <- elaborate' logCtx contentType
      content' <- elaborate' logCtx content
      return $ m :< TermCellIntro contentType' content'
    m :< WeakTermCellRead cell -> do
      cell' <- elaborate' logCtx cell
      return $ m :< TermCellRead cell'
    m :< WeakTermCellWrite cell newValue -> do
      cell' <- elaborate' logCtx cell
      newValue' <- elaborate' logCtx newValue
      return $ m :< TermCellWrite cell' newValue'

-- for now
elaboratePatternList ::
  LogContext IO ->
  Hint ->
  [T.Text] ->
  [(PatternF WeakTerm, WeakTerm)] ->
  IO [(PatternF Term, Term)]
elaboratePatternList logCtx m bs patList = do
  patList' <- forM patList $ \((mPat, c, xts), body) -> do
    xts' <- mapM (elaborateWeakBinder logCtx) xts
    body' <- elaborate' logCtx body
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

elaborateWeakBinder :: LogContext IO -> BinderF WeakTerm -> IO (BinderF Term)
elaborateWeakBinder logCtx (m, x, t) = do
  t' <- elaborate' logCtx t
  return (m, x, t')

elaborateKind :: LogContext IO -> LamKindF WeakTerm -> IO (LamKindF Term)
elaborateKind logCtx kind =
  case kind of
    LamKindNormal ->
      return LamKindNormal
    LamKindCons dataName consName consNumber dataType -> do
      dataType' <- elaborate' logCtx dataType
      return $ LamKindCons dataName consName consNumber dataType'
    LamKindFix xt -> do
      xt' <- elaborateWeakBinder logCtx xt
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
