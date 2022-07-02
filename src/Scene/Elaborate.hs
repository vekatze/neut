module Scene.Elaborate
  ( elaborateMain,
    elaborateOther,
  )
where

import Context.App
import qualified Context.Enum as Enum
import qualified Context.Gensym as Gensym
import qualified Context.Log as Log
import qualified Context.Throw as Throw
import Control.Comonad.Cofree
import Control.Monad
import Data.Function
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
import Prelude hiding (log)

elaborateMain :: Axis -> T.Text -> Source -> Either [Stmt] ([QuasiStmt], [EnumInfo]) -> IO [Stmt]
elaborateMain axis mainFunctionName =
  elaborate (elaborateProgramMain axis mainFunctionName)

elaborateOther :: Axis -> Source -> Either [Stmt] ([QuasiStmt], [EnumInfo]) -> IO [Stmt]
elaborateOther axis =
  elaborate (elaborateProgramOther axis)

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

elaborateProgramMain :: Axis -> T.Text -> [QuasiStmt] -> IO [Stmt]
elaborateProgramMain axis mainFunctionName = do
  elaborateProgram axis $ mapM $ inferStmtMain axis mainFunctionName

elaborateProgramOther :: Axis -> [QuasiStmt] -> IO [Stmt]
elaborateProgramOther axis = do
  elaborateProgram axis $ mapM (inferStmtOther axis)

elaborateProgram :: Axis -> ([QuasiStmt] -> IO [QuasiStmt]) -> [QuasiStmt] -> IO [Stmt]
elaborateProgram axis defListInferrer defList = do
  defList' <- mapM (setupDef (axis & gensym)) defList >>= defListInferrer . concat
  -- cs <- readIORef constraintEnv
  -- p "==========================================================="
  -- forM_ cs $ \(e1, e2) -> do
  --   p $ T.unpack $ toText e1
  --   p $ T.unpack $ toText e2
  --   p "---------------------"
  unify (axis & gensym)
  elaborateStmtList axis defList'

setupDef :: Gensym.Axis -> QuasiStmt -> IO [QuasiStmt]
setupDef axis def =
  case def of
    QuasiStmtDefine opacity m f impArgNum xts codType e -> do
      (xts', codType') <- arrangeBinder axis [] xts codType
      insTermTypeEnv f $ m :< WeakTermPi xts' codType'
      modifyIORef' impArgEnvRef $ Map.insert f impArgNum
      modifyIORef' termDefEnvRef $ Map.insert f (opacity, xts', e)
      return [QuasiStmtDefine opacity m f impArgNum xts' codType' e]
    QuasiStmtDefineResource m name discarder copier -> do
      insTermTypeEnv name $ m :< WeakTermTau
      return [QuasiStmtDefineResource m name discarder copier]

inferStmtMain :: Axis -> T.Text -> QuasiStmt -> IO QuasiStmt
inferStmtMain axis mainFunctionName stmt = do
  case stmt of
    QuasiStmtDefine isReducible m x impArgNum xts codType e -> do
      (xts', e', codType') <- inferStmt axis xts e codType
      when (x == mainFunctionName) $
        insConstraintEnv
          (m :< WeakTermPi [] (m :< WeakTermConst "i64"))
          (m :< WeakTermPi xts codType)
      return $ QuasiStmtDefine isReducible m x impArgNum xts' codType' e'
    QuasiStmtDefineResource m name discarder copier ->
      inferDefineResource axis m name discarder copier

inferStmtOther :: Axis -> QuasiStmt -> IO QuasiStmt
inferStmtOther axis stmt = do
  case stmt of
    QuasiStmtDefine isReducible m x impArgNum xts codType e -> do
      (xts', e', codType') <- inferStmt axis xts e codType
      return $ QuasiStmtDefine isReducible m x impArgNum xts' codType' e'
    QuasiStmtDefineResource m name discarder copier ->
      inferDefineResource axis m name discarder copier

inferDefineResource :: Axis -> Hint -> T.Text -> WeakTerm -> WeakTerm -> IO QuasiStmt
inferDefineResource axis m name discarder copier = do
  (discarder', td) <- infer axis discarder
  (copier', tc) <- infer axis copier
  x <- Gensym.newIdentFromText (axis & gensym) "_"
  let botTop = m :< WeakTermPi [(m, x, m :< WeakTermEnum "bottom")] (m :< WeakTermEnum "top")
  let botBot = m :< WeakTermPi [(m, x, m :< WeakTermEnum "bottom")] (m :< WeakTermEnum "bottom")
  insConstraintEnv botTop td
  insConstraintEnv botBot tc
  return $ QuasiStmtDefineResource m name discarder' copier'

inferStmt :: Axis -> [BinderF WeakTerm] -> WeakTerm -> WeakTerm -> IO ([BinderF WeakTerm], WeakTerm, WeakTerm)
inferStmt axis xts e codType = do
  (xts', (e', te)) <- inferBinder axis [] xts e
  codType' <- inferType axis codType
  insConstraintEnv codType' te
  return (xts', e', codType')

elaborateStmtList :: Axis -> [QuasiStmt] -> IO [Stmt]
elaborateStmtList axis stmtList = do
  case stmtList of
    [] ->
      return []
    QuasiStmtDefine opacity m x impArgNum xts codType e : rest -> do
      e' <- elaborate' axis e
      xts' <- mapM (elaborateWeakBinder axis) xts
      codType' <- elaborate' axis codType >>= Term.reduce (axis & gensym)
      insTermTypeEnv x $ weaken $ m :< TermPi xts' codType'
      modifyIORef' termDefEnvRef $ Map.insert x (opacity, map weakenBinder xts', weaken e')
      rest' <- elaborateStmtList axis rest
      return $ StmtDefine opacity m x impArgNum xts' codType' e' : rest'
    QuasiStmtDefineResource m name discarder copier : rest -> do
      discarder' <- elaborate' axis discarder
      copier' <- elaborate' axis copier
      rest' <- elaborateStmtList axis rest
      return $ StmtDefineResource m name discarder' copier' : rest'

elaborate' :: Axis -> WeakTerm -> IO Term
elaborate' axis term =
  case term of
    m :< WeakTermTau ->
      return $ m :< TermTau
    m :< WeakTermVar x ->
      return $ m :< TermVar x
    m :< WeakTermVarGlobal name ->
      return $ m :< TermVarGlobal name
    m :< WeakTermPi xts t -> do
      xts' <- mapM (elaborateWeakBinder axis) xts
      t' <- elaborate' axis t
      return $ m :< TermPi xts' t'
    m :< WeakTermPiIntro kind xts e -> do
      kind' <- elaborateKind axis kind
      xts' <- mapM (elaborateWeakBinder axis) xts
      e' <- elaborate' axis e
      return $ m :< TermPiIntro kind' xts' e'
    m :< WeakTermPiElim (mh :< WeakTermAster x) es -> do
      subst <- readIORef substRef
      case IntMap.lookup x subst of
        Nothing ->
          (axis & throw & Throw.raiseError) mh "couldn't instantiate the asterisk here"
        Just (_ :< WeakTermPiIntro LamKindNormal xts e)
          | length xts == length es -> do
            let xs = map (\(_, y, _) -> Ident.toInt y) xts
            let s = IntMap.fromList $ zip xs es
            WeakTerm.subst (axis & gensym) s e >>= elaborate' axis
        Just e ->
          WeakTerm.reduce (axis & gensym) (m :< WeakTermPiElim e es) >>= elaborate' axis
    m :< WeakTermPiElim e es -> do
      e' <- elaborate' axis e
      es' <- mapM (elaborate' axis) es
      return $ m :< TermPiElim e' es'
    m :< WeakTermSigma xts -> do
      xts' <- mapM (elaborateWeakBinder axis) xts
      return $ m :< TermSigma xts'
    m :< WeakTermSigmaIntro es -> do
      es' <- mapM (elaborate' axis) es
      return $ m :< TermSigmaIntro es'
    m :< WeakTermSigmaElim xts e1 e2 -> do
      e1' <- elaborate' axis e1
      xts' <- mapM (elaborateWeakBinder axis) xts
      e2' <- elaborate' axis e2
      return $ m :< TermSigmaElim xts' e1' e2'
    m :< WeakTermLet mxt e1 e2 -> do
      e1' <- elaborate' axis e1
      mxt' <- elaborateWeakBinder axis mxt
      e2' <- elaborate' axis e2
      return $ m :< TermLet mxt' e1' e2'
    m :< WeakTermAster _ ->
      (axis & throw & Throw.raiseCritical)
        m
        "every meta-variable must be of the form (?M e1 ... en) where n >= 0, but the meta-variable here doesn't fit this pattern"
    m :< WeakTermConst x ->
      return $ m :< TermConst x
    m :< WeakTermInt t x -> do
      t' <- elaborate' axis t >>= Term.reduce (axis & gensym)
      case t' of
        _ :< TermConst intTypeStr
          | Just (PrimNumInt size) <- PrimNum.fromText intTypeStr ->
            return $ m :< TermInt size x
        _ -> do
          (axis & throw & Throw.raiseError) m $
            "the term `"
              <> T.pack (show x)
              <> "` is an integer, but its type is: "
              <> toText (weaken t')
    m :< WeakTermFloat t x -> do
      t' <- elaborate' axis t >>= Term.reduce (axis & gensym)
      case t' of
        _ :< TermConst floatTypeStr
          | Just (PrimNumFloat size) <- PrimNum.fromText floatTypeStr ->
            return $ m :< TermFloat size x
        _ ->
          (axis & throw & Throw.raiseError) m $
            "the term `"
              <> T.pack (show x)
              <> "` is a float, but its type is:\n"
              <> toText (weaken t')
    m :< WeakTermEnum k ->
      return $ m :< TermEnum k
    m :< WeakTermEnumIntro labelInfo x ->
      return $ m :< TermEnumIntro labelInfo x
    m :< WeakTermEnumElim (e, t) les -> do
      e' <- elaborate' axis e
      let (ls, es) = unzip les
      es' <- mapM (elaborate' axis) es
      t' <- elaborate' axis t >>= Term.reduce (axis & gensym)
      case t' of
        _ :< TermEnum x -> do
          checkSwitchExaustiveness axis m x ls
          return $ m :< TermEnumElim (e', t') (zip ls es')
        _ ->
          (axis & throw & Throw.raiseError) m $
            "the type of `"
              <> toText (weaken e')
              <> "` must be an enum type, but is:\n"
              <> toText (weaken t')
    m :< WeakTermQuestion e t -> do
      e' <- elaborate' axis e
      t' <- elaborate' axis t
      (axis & log & Log.printNote) m $ toText (weaken t')
      return e'
    m :< WeakTermMagic der -> do
      der' <- mapM (elaborate' axis) der
      return $ m :< TermMagic der'
    m :< WeakTermMatch mSubject (e, t) patList -> do
      mSubject' <- mapM (elaborate' axis) mSubject
      e' <- elaborate' axis e
      t' <- elaborate' axis t >>= Term.reduce (axis & gensym)
      dataEnv <- readIORef dataEnvRef
      case t' of
        _ :< TermPiElim (_ :< TermVarGlobal name) _
          | Just bs <- Map.lookup name dataEnv -> do
            patList' <- elaboratePatternList axis m bs patList
            return $ m :< TermMatch mSubject' (e', t') patList'
        _ :< TermVarGlobal name
          | Just bs <- Map.lookup name dataEnv -> do
            patList' <- elaboratePatternList axis m bs patList
            return $ m :< TermMatch mSubject' (e', t') patList'
        _ -> do
          (axis & throw & Throw.raiseError) (metaOf t) $
            "the type of this term must be a data-type, but its type is:\n" <> toText (weaken t')
    m :< WeakTermNoema s e -> do
      s' <- elaborate' axis s
      e' <- elaborate' axis e
      return $ m :< TermNoema s' e'
    m :< WeakTermNoemaIntro s e -> do
      e' <- elaborate' axis e
      return $ m :< TermNoemaIntro s e'
    m :< WeakTermNoemaElim s e -> do
      e' <- elaborate' axis e
      return $ m :< TermNoemaElim s e'
    m :< WeakTermArray elemType -> do
      elemType' <- elaborate' axis elemType
      case elemType' of
        _ :< TermConst typeStr
          | Just (PrimNumInt size) <- PrimNum.fromText typeStr ->
            return $ m :< TermArray (PrimNumInt size)
          | Just (PrimNumFloat size) <- PrimNum.fromText typeStr ->
            return $ m :< TermArray (PrimNumFloat size)
        _ ->
          (axis & throw & Throw.raiseError) m $
            "invalid element type:\n" <> toText (weaken elemType')
    m :< WeakTermArrayIntro elemType elems -> do
      elemType' <- elaborate' axis elemType
      elems' <- mapM (elaborate' axis) elems
      case elemType' of
        _ :< TermConst typeStr
          | Just (PrimNumInt size) <- PrimNum.fromText typeStr ->
            return $ m :< TermArrayIntro (PrimNumInt size) elems'
          | Just (PrimNumFloat size) <- PrimNum.fromText typeStr ->
            return $ m :< TermArrayIntro (PrimNumFloat size) elems'
        _ ->
          (axis & throw & Throw.raiseError) m $ "invalid element type:\n" <> toText (weaken elemType')
    m :< WeakTermArrayAccess subject elemType array index -> do
      subject' <- elaborate' axis subject
      elemType' <- elaborate' axis elemType
      array' <- elaborate' axis array
      index' <- elaborate' axis index
      case elemType' of
        _ :< TermConst typeStr
          | Just (PrimNumInt size) <- PrimNum.fromText typeStr ->
            return $ m :< TermArrayAccess subject' (PrimNumInt size) array' index'
          | Just (PrimNumFloat size) <- PrimNum.fromText typeStr ->
            return $ m :< TermArrayAccess subject' (PrimNumFloat size) array' index'
        _ ->
          (axis & throw & Throw.raiseError) m $ "invalid element type:\n" <> toText (weaken elemType')
    m :< WeakTermText ->
      return $ m :< TermText
    m :< WeakTermTextIntro text ->
      return $ m :< TermTextIntro text
    m :< WeakTermCell contentType -> do
      contentType' <- elaborate' axis contentType
      return $ m :< TermCell contentType'
    m :< WeakTermCellIntro contentType content -> do
      contentType' <- elaborate' axis contentType
      content' <- elaborate' axis content
      return $ m :< TermCellIntro contentType' content'
    m :< WeakTermCellRead cell -> do
      cell' <- elaborate' axis cell
      return $ m :< TermCellRead cell'
    m :< WeakTermCellWrite cell newValue -> do
      cell' <- elaborate' axis cell
      newValue' <- elaborate' axis newValue
      return $ m :< TermCellWrite cell' newValue'

-- for now
elaboratePatternList ::
  Axis ->
  Hint ->
  [T.Text] ->
  [(PatternF WeakTerm, WeakTerm)] ->
  IO [(PatternF Term, Term)]
elaboratePatternList axis m bs patList = do
  patList' <- forM patList $ \((mPat, c, xts), body) -> do
    xts' <- mapM (elaborateWeakBinder axis) xts
    body' <- elaborate' axis body
    return ((mPat, c, xts'), body')
  checkCaseSanity axis m bs patList'
  return patList'

checkCaseSanity :: Axis -> Hint -> [T.Text] -> [(PatternF Term, Term)] -> IO ()
checkCaseSanity axis m bs patList =
  case (bs, patList) of
    ([], []) ->
      return ()
    (b : bsRest, ((mPat, b', _), _) : patListRest) -> do
      if b /= b'
        then
          (axis & throw & Throw.raiseError) mPat $
            "the constructor here is supposed to be `" <> b <> "`, but is: `" <> b' <> "`"
        else checkCaseSanity axis m bsRest patListRest
    (b : _, []) ->
      (axis & throw & Throw.raiseError) m $
        "found a non-exhaustive pattern; the clause for `" <> b <> "` is missing"
    ([], ((mPat, b, _), _) : _) ->
      (axis & throw & Throw.raiseError) mPat $
        "found a redundant pattern; this clause for `" <> b <> "` is redundant"

elaborateWeakBinder :: Axis -> BinderF WeakTerm -> IO (BinderF Term)
elaborateWeakBinder axis (m, x, t) = do
  t' <- elaborate' axis t
  return (m, x, t')

elaborateKind :: Axis -> LamKindF WeakTerm -> IO (LamKindF Term)
elaborateKind axis kind =
  case kind of
    LamKindNormal ->
      return LamKindNormal
    LamKindCons dataName consName consNumber dataType -> do
      dataType' <- elaborate' axis dataType
      return $ LamKindCons dataName consName consNumber dataType'
    LamKindFix xt -> do
      xt' <- elaborateWeakBinder axis xt
      return $ LamKindFix xt'

checkSwitchExaustiveness :: Axis -> Hint -> T.Text -> [EnumCase] -> IO ()
checkSwitchExaustiveness axis m x caseList = do
  let containsDefaultCase = doesContainDefaultCase caseList
  enumSet <- lookupEnumSet axis m x
  let len = toInteger $ length (nub caseList)
  unless (toInteger (length enumSet) <= len || containsDefaultCase) $
    (axis & throw & Throw.raiseError) m "this switch is ill-constructed in that it is not exhaustive"

lookupEnumSet :: Axis -> Hint -> T.Text -> IO [T.Text]
lookupEnumSet axis m name = do
  mEnumItems <- Enum.lookupType (axis & enum) name
  case mEnumItems of
    Nothing ->
      (axis & throw & Throw.raiseError) m $ "no such enum defined: " <> name
    Just enumItems ->
      return $ map fst enumItems

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
