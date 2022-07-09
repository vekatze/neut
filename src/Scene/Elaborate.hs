module Scene.Elaborate
  ( elaborateMain,
    elaborateOther,
  )
where

import Context.App
import qualified Context.Gensym as Gensym
import qualified Context.Global as Global
import qualified Context.Log as Log
import qualified Context.Throw as Throw
import Control.Comonad.Cofree
import Control.Monad
import qualified Data.HashMap.Strict as Map
import Data.IORef
import qualified Data.IntMap as IntMap
import Data.List
import qualified Data.Text as T
import Entity.Binder
import Entity.EnumCase
import Entity.EnumInfo
import qualified Entity.EnumTypeName as ET
import qualified Entity.EnumValueName as EV
import Entity.Global
import qualified Entity.GlobalName as GN
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

elaborateMain :: Context -> T.Text -> Source -> Either [Stmt] ([QuasiStmt], [EnumInfo]) -> IO [Stmt]
elaborateMain ctx mainFunctionName =
  elaborate (elaborateProgramMain ctx mainFunctionName)

elaborateOther :: Context -> Source -> Either [Stmt] ([QuasiStmt], [EnumInfo]) -> IO [Stmt]
elaborateOther ctx =
  elaborate (elaborateProgramOther ctx)

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

elaborateProgramMain :: Context -> T.Text -> [QuasiStmt] -> IO [Stmt]
elaborateProgramMain ctx mainFunctionName = do
  elaborateProgram ctx $ mapM $ inferStmtMain ctx mainFunctionName

elaborateProgramOther :: Context -> [QuasiStmt] -> IO [Stmt]
elaborateProgramOther ctx = do
  elaborateProgram ctx $ mapM (inferStmtOther ctx)

elaborateProgram :: Context -> ([QuasiStmt] -> IO [QuasiStmt]) -> [QuasiStmt] -> IO [Stmt]
elaborateProgram ctx defListInferrer defList = do
  defList' <- mapM (setupDef (gensym ctx)) defList >>= defListInferrer . concat
  -- cs <- readIORef constraintEnv
  -- p "==========================================================="
  -- forM_ cs $ \(e1, e2) -> do
  --   p $ T.unpack $ toText e1
  --   p $ T.unpack $ toText e2
  --   p "---------------------"
  unify (gensym ctx)
  elaborateStmtList ctx defList'

setupDef :: Gensym.Context -> QuasiStmt -> IO [QuasiStmt]
setupDef ctx def =
  case def of
    QuasiStmtDefine opacity m f impArgNum xts codType e -> do
      (xts', codType') <- arrangeBinder ctx [] xts codType
      insTermTypeEnv f $ m :< WeakTermPi xts' codType'
      modifyIORef' impArgEnvRef $ Map.insert f impArgNum
      modifyIORef' termDefEnvRef $ Map.insert f (opacity, xts', e)
      return [QuasiStmtDefine opacity m f impArgNum xts' codType' e]
    QuasiStmtDefineResource m name discarder copier -> do
      insTermTypeEnv name $ m :< WeakTermTau
      return [QuasiStmtDefineResource m name discarder copier]

inferStmtMain :: Context -> T.Text -> QuasiStmt -> IO QuasiStmt
inferStmtMain ctx mainFunctionName stmt = do
  case stmt of
    QuasiStmtDefine isReducible m x impArgNum xts codType e -> do
      (xts', e', codType') <- inferStmt ctx xts e codType
      when (x == mainFunctionName) $
        insConstraintEnv
          (m :< WeakTermPi [] (m :< WeakTermPrim "i64"))
          (m :< WeakTermPi xts codType)
      return $ QuasiStmtDefine isReducible m x impArgNum xts' codType' e'
    QuasiStmtDefineResource m name discarder copier ->
      inferDefineResource ctx m name discarder copier

inferStmtOther :: Context -> QuasiStmt -> IO QuasiStmt
inferStmtOther ctx stmt = do
  case stmt of
    QuasiStmtDefine isReducible m x impArgNum xts codType e -> do
      (xts', e', codType') <- inferStmt ctx xts e codType
      return $ QuasiStmtDefine isReducible m x impArgNum xts' codType' e'
    QuasiStmtDefineResource m name discarder copier ->
      inferDefineResource ctx m name discarder copier

inferDefineResource :: Context -> Hint -> T.Text -> WeakTerm -> WeakTerm -> IO QuasiStmt
inferDefineResource ctx m name discarder copier = do
  (discarder', td) <- infer ctx discarder
  (copier', tc) <- infer ctx copier
  x <- Gensym.newIdentFromText (gensym ctx) "_"
  let botTop = m :< WeakTermPi [(m, x, m :< WeakTermEnum constBottom)] (m :< WeakTermEnum constTop)
  let botBot = m :< WeakTermPi [(m, x, m :< WeakTermEnum constBottom)] (m :< WeakTermEnum constBottom)
  insConstraintEnv botTop td
  insConstraintEnv botBot tc
  return $ QuasiStmtDefineResource m name discarder' copier'

inferStmt :: Context -> [BinderF WeakTerm] -> WeakTerm -> WeakTerm -> IO ([BinderF WeakTerm], WeakTerm, WeakTerm)
inferStmt ctx xts e codType = do
  (xts', (e', te)) <- inferBinder ctx [] xts e
  codType' <- inferType ctx codType
  insConstraintEnv codType' te
  return (xts', e', codType')

elaborateStmtList :: Context -> [QuasiStmt] -> IO [Stmt]
elaborateStmtList ctx stmtList = do
  case stmtList of
    [] ->
      return []
    QuasiStmtDefine opacity m x impArgNum xts codType e : rest -> do
      e' <- elaborate' ctx e
      xts' <- mapM (elaborateWeakBinder ctx) xts
      codType' <- elaborate' ctx codType >>= Term.reduce (gensym ctx)
      insTermTypeEnv x $ weaken $ m :< TermPi xts' codType'
      modifyIORef' termDefEnvRef $ Map.insert x (opacity, map weakenBinder xts', weaken e')
      rest' <- elaborateStmtList ctx rest
      return $ StmtDefine opacity m x impArgNum xts' codType' e' : rest'
    QuasiStmtDefineResource m name discarder copier : rest -> do
      discarder' <- elaborate' ctx discarder
      copier' <- elaborate' ctx copier
      rest' <- elaborateStmtList ctx rest
      return $ StmtDefineResource m name discarder' copier' : rest'

elaborate' :: Context -> WeakTerm -> IO Term
elaborate' ctx term =
  case term of
    m :< WeakTermTau ->
      return $ m :< TermTau
    m :< WeakTermVar x ->
      return $ m :< TermVar x
    m :< WeakTermVarGlobal name ->
      return $ m :< TermVarGlobal name
    m :< WeakTermPi xts t -> do
      xts' <- mapM (elaborateWeakBinder ctx) xts
      t' <- elaborate' ctx t
      return $ m :< TermPi xts' t'
    m :< WeakTermPiIntro kind xts e -> do
      kind' <- elaborateKind ctx kind
      xts' <- mapM (elaborateWeakBinder ctx) xts
      e' <- elaborate' ctx e
      return $ m :< TermPiIntro kind' xts' e'
    m :< WeakTermPiElim (mh :< WeakTermAster x) es -> do
      subst <- readIORef substRef
      case IntMap.lookup x subst of
        Nothing ->
          Throw.raiseError (throw ctx) mh "couldn't instantiate the asterisk here"
        Just (_ :< WeakTermPiIntro LamKindNormal xts e)
          | length xts == length es -> do
            let xs = map (\(_, y, _) -> Ident.toInt y) xts
            let s = IntMap.fromList $ zip xs es
            WeakTerm.subst (gensym ctx) s e >>= elaborate' ctx
        Just e ->
          WeakTerm.reduce (gensym ctx) (m :< WeakTermPiElim e es) >>= elaborate' ctx
    m :< WeakTermPiElim e es -> do
      e' <- elaborate' ctx e
      es' <- mapM (elaborate' ctx) es
      return $ m :< TermPiElim e' es'
    m :< WeakTermSigma xts -> do
      xts' <- mapM (elaborateWeakBinder ctx) xts
      return $ m :< TermSigma xts'
    m :< WeakTermSigmaIntro es -> do
      es' <- mapM (elaborate' ctx) es
      return $ m :< TermSigmaIntro es'
    m :< WeakTermSigmaElim xts e1 e2 -> do
      e1' <- elaborate' ctx e1
      xts' <- mapM (elaborateWeakBinder ctx) xts
      e2' <- elaborate' ctx e2
      return $ m :< TermSigmaElim xts' e1' e2'
    m :< WeakTermLet mxt e1 e2 -> do
      e1' <- elaborate' ctx e1
      mxt' <- elaborateWeakBinder ctx mxt
      e2' <- elaborate' ctx e2
      return $ m :< TermLet mxt' e1' e2'
    m :< WeakTermAster _ ->
      Throw.raiseCritical
        (throw ctx)
        m
        "every meta-variable must be of the form (?M e1 ... en) where n >= 0, but the meta-variable here doesn't fit this pattern"
    m :< WeakTermPrim x ->
      return $ m :< TermPrim x
    m :< WeakTermInt t x -> do
      t' <- elaborate' ctx t >>= Term.reduce (gensym ctx)
      case t' of
        _ :< TermPrim intTypeStr
          | Just (PrimNumInt size) <- PrimNum.fromText intTypeStr ->
            return $ m :< TermInt size x
        _ -> do
          Throw.raiseError (throw ctx) m $
            "the term `"
              <> T.pack (show x)
              <> "` is an integer, but its type is: "
              <> toText (weaken t')
    m :< WeakTermFloat t x -> do
      t' <- elaborate' ctx t >>= Term.reduce (gensym ctx)
      case t' of
        _ :< TermPrim floatTypeStr
          | Just (PrimNumFloat size) <- PrimNum.fromText floatTypeStr ->
            return $ m :< TermFloat size x
        _ ->
          Throw.raiseError (throw ctx) m $
            "the term `"
              <> T.pack (show x)
              <> "` is a float, but its type is:\n"
              <> toText (weaken t')
    m :< WeakTermEnum k ->
      return $ m :< TermEnum k
    m :< WeakTermEnumIntro labelInfo x ->
      return $ m :< TermEnumIntro labelInfo x
    m :< WeakTermEnumElim (e, t) les -> do
      e' <- elaborate' ctx e
      let (ls, es) = unzip les
      es' <- mapM (elaborate' ctx) es
      t' <- elaborate' ctx t >>= Term.reduce (gensym ctx)
      case t' of
        _ :< TermEnum x -> do
          checkSwitchExaustiveness ctx m x ls
          return $ m :< TermEnumElim (e', t') (zip ls es')
        _ ->
          Throw.raiseError (throw ctx) m $
            "the type of `"
              <> toText (weaken e')
              <> "` must be an enum type, but is:\n"
              <> toText (weaken t')
    m :< WeakTermQuestion e t -> do
      e' <- elaborate' ctx e
      t' <- elaborate' ctx t
      Log.printNote (log ctx) m $ toText (weaken t')
      return e'
    m :< WeakTermMagic der -> do
      der' <- mapM (elaborate' ctx) der
      return $ m :< TermMagic der'
    m :< WeakTermMatch mSubject (e, t) patList -> do
      mSubject' <- mapM (elaborate' ctx) mSubject
      e' <- elaborate' ctx e
      t' <- elaborate' ctx t >>= Term.reduce (gensym ctx)
      dataEnv <- readIORef dataEnvRef
      case t' of
        _ :< TermPiElim (_ :< TermVarGlobal name) _
          | Just bs <- Map.lookup name dataEnv -> do
            patList' <- elaboratePatternList ctx m bs patList
            return $ m :< TermMatch mSubject' (e', t') patList'
        _ :< TermVarGlobal name
          | Just bs <- Map.lookup name dataEnv -> do
            patList' <- elaboratePatternList ctx m bs patList
            return $ m :< TermMatch mSubject' (e', t') patList'
        _ -> do
          Throw.raiseError (throw ctx) (metaOf t) $
            "the type of this term must be a data-type, but its type is:\n" <> toText (weaken t')
    m :< WeakTermNoema s e -> do
      s' <- elaborate' ctx s
      e' <- elaborate' ctx e
      return $ m :< TermNoema s' e'
    m :< WeakTermNoemaIntro s e -> do
      e' <- elaborate' ctx e
      return $ m :< TermNoemaIntro s e'
    m :< WeakTermNoemaElim s e -> do
      e' <- elaborate' ctx e
      return $ m :< TermNoemaElim s e'
    m :< WeakTermArray elemType -> do
      elemType' <- elaborate' ctx elemType
      case elemType' of
        _ :< TermPrim typeStr
          | Just (PrimNumInt size) <- PrimNum.fromText typeStr ->
            return $ m :< TermArray (PrimNumInt size)
          | Just (PrimNumFloat size) <- PrimNum.fromText typeStr ->
            return $ m :< TermArray (PrimNumFloat size)
        _ ->
          Throw.raiseError (throw ctx) m $
            "invalid element type:\n" <> toText (weaken elemType')
    m :< WeakTermArrayIntro elemType elems -> do
      elemType' <- elaborate' ctx elemType
      elems' <- mapM (elaborate' ctx) elems
      case elemType' of
        _ :< TermPrim typeStr
          | Just (PrimNumInt size) <- PrimNum.fromText typeStr ->
            return $ m :< TermArrayIntro (PrimNumInt size) elems'
          | Just (PrimNumFloat size) <- PrimNum.fromText typeStr ->
            return $ m :< TermArrayIntro (PrimNumFloat size) elems'
        _ ->
          Throw.raiseError (throw ctx) m $ "invalid element type:\n" <> toText (weaken elemType')
    m :< WeakTermArrayAccess subject elemType array index -> do
      subject' <- elaborate' ctx subject
      elemType' <- elaborate' ctx elemType
      array' <- elaborate' ctx array
      index' <- elaborate' ctx index
      case elemType' of
        _ :< TermPrim typeStr
          | Just (PrimNumInt size) <- PrimNum.fromText typeStr ->
            return $ m :< TermArrayAccess subject' (PrimNumInt size) array' index'
          | Just (PrimNumFloat size) <- PrimNum.fromText typeStr ->
            return $ m :< TermArrayAccess subject' (PrimNumFloat size) array' index'
        _ ->
          Throw.raiseError (throw ctx) m $ "invalid element type:\n" <> toText (weaken elemType')
    m :< WeakTermText ->
      return $ m :< TermText
    m :< WeakTermTextIntro text ->
      return $ m :< TermTextIntro text
    m :< WeakTermCell contentType -> do
      contentType' <- elaborate' ctx contentType
      return $ m :< TermCell contentType'
    m :< WeakTermCellIntro contentType content -> do
      contentType' <- elaborate' ctx contentType
      content' <- elaborate' ctx content
      return $ m :< TermCellIntro contentType' content'
    m :< WeakTermCellRead cell -> do
      cell' <- elaborate' ctx cell
      return $ m :< TermCellRead cell'
    m :< WeakTermCellWrite cell newValue -> do
      cell' <- elaborate' ctx cell
      newValue' <- elaborate' ctx newValue
      return $ m :< TermCellWrite cell' newValue'

-- for now
elaboratePatternList ::
  Context ->
  Hint ->
  [T.Text] ->
  [(PatternF WeakTerm, WeakTerm)] ->
  IO [(PatternF Term, Term)]
elaboratePatternList ctx m bs patList = do
  patList' <- forM patList $ \((mPat, c, xts), body) -> do
    xts' <- mapM (elaborateWeakBinder ctx) xts
    body' <- elaborate' ctx body
    return ((mPat, c, xts'), body')
  checkCaseSanity ctx m bs patList'
  return patList'

checkCaseSanity :: Context -> Hint -> [T.Text] -> [(PatternF Term, Term)] -> IO ()
checkCaseSanity ctx m bs patList =
  case (bs, patList) of
    ([], []) ->
      return ()
    (b : bsRest, ((mPat, b', _), _) : patListRest) -> do
      if b /= b'
        then
          Throw.raiseError (throw ctx) mPat $
            "the constructor here is supposed to be `" <> b <> "`, but is: `" <> b' <> "`"
        else checkCaseSanity ctx m bsRest patListRest
    (b : _, []) ->
      Throw.raiseError (throw ctx) m $
        "found a non-exhaustive pattern; the clause for `" <> b <> "` is missing"
    ([], ((mPat, b, _), _) : _) ->
      Throw.raiseError (throw ctx) mPat $
        "found a redundant pattern; this clause for `" <> b <> "` is redundant"

elaborateWeakBinder :: Context -> BinderF WeakTerm -> IO (BinderF Term)
elaborateWeakBinder ctx (m, x, t) = do
  t' <- elaborate' ctx t
  return (m, x, t')

elaborateKind :: Context -> LamKindF WeakTerm -> IO (LamKindF Term)
elaborateKind ctx kind =
  case kind of
    LamKindNormal ->
      return LamKindNormal
    LamKindCons dataName consName consNumber dataType -> do
      dataType' <- elaborate' ctx dataType
      return $ LamKindCons dataName consName consNumber dataType'
    LamKindFix xt -> do
      xt' <- elaborateWeakBinder ctx xt
      return $ LamKindFix xt'

checkSwitchExaustiveness :: Context -> Hint -> ET.EnumTypeName -> [EnumCase] -> IO ()
checkSwitchExaustiveness ctx m enumTypeName caseList = do
  let containsDefaultCase = doesContainDefaultCase caseList
  enumSet <- lookupEnumSet ctx m enumTypeName
  let len = toInteger $ length (nub caseList)
  unless (toInteger (length enumSet) <= len || containsDefaultCase) $
    Throw.raiseError (throw ctx) m "this switch is ill-constructed in that it is not exhaustive"

lookupEnumSet :: Context -> Hint -> ET.EnumTypeName -> IO [EV.EnumValueName]
lookupEnumSet ctx m enumTypeName = do
  let name = ET.reify enumTypeName
  mEnumItems <- Global.lookup (global ctx) name
  case mEnumItems of
    Just (GN.EnumType enumItems) ->
      return $ map fst enumItems
    _ ->
      Throw.raiseError (throw ctx) m $ "no such enum defined: " <> name

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
