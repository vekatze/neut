module Scene.Elaborate
  ( elaborate,
    Context (..),
  )
where

import Control.Comonad.Cofree
import Control.Monad
import qualified Data.IntMap as IntMap
import Data.List
import qualified Data.Text as T
import Entity.Binder
import qualified Entity.DefiniteDescription as DD
import Entity.EnumCase
import Entity.EnumInfo
import qualified Entity.EnumTypeName as ET
import qualified Entity.EnumValueName as EV
import qualified Entity.GlobalName as GN
import Entity.Hint
import qualified Entity.HoleSubst as HS
import qualified Entity.Ident.Reify as Ident
import qualified Entity.ImpArgNum as I
import Entity.LamKind
import Entity.Opacity
import Entity.Pattern
import qualified Entity.Prim as Prim
import Entity.PrimNum
import Entity.Source
import Entity.Stmt
import Entity.Term
import qualified Entity.Term.Reduce as Term
import qualified Entity.Term.Subst as Subst
import Entity.Term.Weaken
import Entity.WeakTerm
import qualified Entity.WeakTerm.Subst as WeakSubst
import qualified Entity.WeakTerm.Subst as WeakTerm
import Entity.WeakTerm.ToText
import qualified Scene.Elaborate.Infer as Infer
import qualified Scene.Elaborate.Unify as Unify
import Prelude hiding (log)

data Context = Context
  { inferCtx :: Infer.Context,
    unifyCtx :: Unify.Context,
    substCtx :: Subst.Context,
    weakSubstCtx :: WeakSubst.Context,
    getConstraintEnv :: IO [(WeakTerm, WeakTerm)],
    getMainDefiniteDescription :: Source -> IO (Maybe DD.DefiniteDescription),
    insertDefinition :: Opacity -> Hint -> DD.DefiniteDescription -> [BinderF WeakTerm] -> WeakTerm -> IO (),
    insertImplictArgNum :: DD.DefiniteDescription -> I.ImpArgNum -> IO (),
    insertWeakType :: DD.DefiniteDescription -> WeakTerm -> IO (),
    putHoleSubst :: HS.HoleSubst -> IO (),
    getHoleSubst :: IO HS.HoleSubst,
    lookupGlobal :: DD.DefiniteDescription -> IO (Maybe GN.GlobalName),
    raiseError :: forall a. Hint -> T.Text -> IO a,
    raiseCritical :: forall a. Hint -> T.Text -> IO a,
    printNote :: Hint -> T.Text -> IO ()
  }

elaborate :: Context -> Source -> Either [Stmt] ([WeakStmt], [EnumInfo]) -> IO [Stmt]
elaborate ctx source cacheOrStmt = do
  case cacheOrStmt of
    Left cache -> do
      forM_ cache $ registerTopLevelDef ctx
      return cache
    Right (defList, enumInfoList) -> do
      mMainDefiniteDescription <- getMainDefiniteDescription ctx source
      -- infer
      defList' <- mapM (setupDef ctx) defList
      defList'' <- mapM (inferStmt (inferCtx ctx) mMainDefiniteDescription) defList'
      constraintList <- getConstraintEnv ctx
      -- unify
      Unify.unify (unifyCtx ctx) constraintList >>= putHoleSubst ctx
      -- elaborate
      defList''' <- elaborateStmtList ctx defList''
      saveCache (source, defList''') enumInfoList
      return defList'''

registerTopLevelDef :: Context -> Stmt -> IO ()
registerTopLevelDef ctx stmt = do
  case stmt of
    StmtDefine opacity m x impArgNum xts codType e -> do
      insertImplictArgNum ctx x impArgNum
      insertWeakType ctx x $ weaken $ m :< TermPi xts codType
      insertDefinition ctx opacity m x (map weakenBinder xts) (weaken e)
    StmtDefineResource m name _ _ ->
      insertWeakType ctx name $ m :< WeakTermTau

setupDef :: Context -> WeakStmt -> IO WeakStmt
setupDef ctx def =
  case def of
    WeakStmtDefine opacity m f impArgNum xts codType e -> do
      insertWeakType ctx f $ m :< WeakTermPi xts codType
      insertImplictArgNum ctx f impArgNum
      insertDefinition ctx opacity m f xts e
      return $ WeakStmtDefine opacity m f impArgNum xts codType e
    WeakStmtDefineResource m name discarder copier -> do
      insertWeakType ctx name $ m :< WeakTermTau
      return $ WeakStmtDefineResource m name discarder copier

inferStmt :: Infer.Context -> Maybe DD.DefiniteDescription -> WeakStmt -> IO WeakStmt
inferStmt ctx mMainDD stmt = do
  case stmt of
    WeakStmtDefine isReducible m x impArgNum xts codType e -> do
      (xts', e', codType') <- inferStmtDefine ctx xts e codType
      when (Just x == mMainDD) $
        Infer.insConstraintEnv ctx (m :< WeakTermPi [] (i64 m)) (m :< WeakTermPi xts codType)
      return $ WeakStmtDefine isReducible m x impArgNum xts' codType' e'
    WeakStmtDefineResource m name discarder copier ->
      Infer.inferDefineResource ctx m name discarder copier

inferStmtDefine ::
  Infer.Context ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  WeakTerm ->
  IO ([BinderF WeakTerm], WeakTerm, WeakTerm)
inferStmtDefine ctx xts e codType = do
  (xts', (e', te)) <- Infer.inferBinder ctx [] xts e
  codType' <- Infer.inferType ctx codType
  Infer.insConstraintEnv ctx codType' te
  return (xts', e', codType')

elaborateStmtList :: Context -> [WeakStmt] -> IO [Stmt]
elaborateStmtList ctx stmtList = do
  case stmtList of
    [] ->
      return []
    WeakStmtDefine opacity m x impArgNum xts codType e : rest -> do
      e' <- elaborate' ctx e
      xts' <- mapM (elaborateWeakBinder ctx) xts
      codType' <- elaborate' ctx codType >>= Term.reduce (substCtx ctx)
      insertWeakType ctx x $ weaken $ m :< TermPi xts' codType'
      insertDefinition ctx opacity m x (map weakenBinder xts') (weaken e')
      rest' <- elaborateStmtList ctx rest
      return $ StmtDefine opacity m x impArgNum xts' codType' e' : rest'
    WeakStmtDefineResource m name discarder copier : rest -> do
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
    m :< WeakTermVarGlobal name arity ->
      return $ m :< TermVarGlobal name arity
    m :< WeakTermPi xts t -> do
      xts' <- mapM (elaborateWeakBinder ctx) xts
      t' <- elaborate' ctx t
      return $ m :< TermPi xts' t'
    m :< WeakTermPiIntro kind xts e -> do
      kind' <- elaborateKind ctx kind
      xts' <- mapM (elaborateWeakBinder ctx) xts
      e' <- elaborate' ctx e
      return $ m :< TermPiIntro kind' xts' e'
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
    m :< WeakTermAster h es -> do
      holeSubst <- getHoleSubst ctx
      case HS.lookup h holeSubst of
        Nothing ->
          raiseError ctx m "couldn't instantiate the hole here"
        Just (xs, e)
          | length xs == length es -> do
            let s = IntMap.fromList $ zip (map Ident.toInt xs) es
            WeakTerm.subst (weakSubstCtx ctx) s e >>= elaborate' ctx
          | otherwise ->
            raiseError ctx m "arity mismatch"
    m :< WeakTermPrim x ->
      return $ m :< TermPrim x
    m :< WeakTermInt t x -> do
      t' <- elaborate' ctx t >>= Term.reduce (substCtx ctx)
      case t' of
        _ :< TermPrim (Prim.Type (PrimNumInt size)) ->
          return $ m :< TermInt size x
        _ -> do
          raiseError ctx m $
            "the term `"
              <> T.pack (show x)
              <> "` is an integer, but its type is: "
              <> toText (weaken t')
    m :< WeakTermFloat t x -> do
      t' <- elaborate' ctx t >>= Term.reduce (substCtx ctx)
      case t' of
        _ :< TermPrim (Prim.Type (PrimNumFloat size)) ->
          return $ m :< TermFloat size x
        _ ->
          raiseError ctx m $
            "the term `"
              <> T.pack (show x)
              <> "` is a float, but its type is:\n"
              <> toText (weaken t')
    m :< WeakTermEnum k ->
      return $ m :< TermEnum k
    m :< WeakTermEnumIntro label ->
      return $ m :< TermEnumIntro label
    m :< WeakTermEnumElim (e, t) les -> do
      e' <- elaborate' ctx e
      let (ls, es) = unzip les
      es' <- mapM (elaborate' ctx) es
      t' <- elaborate' ctx t >>= Term.reduce (substCtx ctx)
      case t' of
        _ :< TermEnum x -> do
          checkSwitchExaustiveness ctx m x ls
          return $ m :< TermEnumElim (e', t') (zip ls es')
        _ ->
          raiseError ctx m $
            "the type of `"
              <> toText (weaken e')
              <> "` must be an enum type, but is:\n"
              <> toText (weaken t')
    m :< WeakTermQuestion e t -> do
      e' <- elaborate' ctx e
      t' <- elaborate' ctx t
      printNote ctx m $ toText (weaken t')
      return e'
    m :< WeakTermMagic der -> do
      der' <- mapM (elaborate' ctx) der
      return $ m :< TermMagic der'
    m :< WeakTermMatch mSubject (e, t) patList -> do
      mSubject' <- mapM (elaborate' ctx) mSubject
      e' <- elaborate' ctx e
      t' <- elaborate' ctx t >>= Term.reduce (substCtx ctx)
      case t' of
        _ :< TermPiElim (_ :< TermVarGlobal name _) _ -> do
          mConsInfoList <- lookupGlobal ctx name
          case mConsInfoList of
            Just (GN.Data _ consInfoList) -> do
              patList' <- elaboratePatternList ctx m consInfoList patList
              return $ m :< TermMatch mSubject' (e', t') patList'
            _ ->
              raiseError ctx (metaOf t) $
                "the type of this term must be a data-type, but its type is:\n" <> toText (weaken t')
        _ -> do
          raiseError ctx (metaOf t) $
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
        _ :< TermPrim (Prim.Type (PrimNumInt size)) ->
          return $ m :< TermArray (PrimNumInt size)
        _ :< TermPrim (Prim.Type (PrimNumFloat size)) ->
          return $ m :< TermArray (PrimNumFloat size)
        _ ->
          raiseError ctx m $
            "invalid element type:\n" <> toText (weaken elemType')
    m :< WeakTermArrayIntro elemType elems -> do
      elemType' <- elaborate' ctx elemType
      elems' <- mapM (elaborate' ctx) elems
      case elemType' of
        _ :< TermPrim (Prim.Type (PrimNumInt size)) ->
          return $ m :< TermArrayIntro (PrimNumInt size) elems'
        _ :< TermPrim (Prim.Type (PrimNumFloat size)) ->
          return $ m :< TermArrayIntro (PrimNumFloat size) elems'
        _ ->
          raiseError ctx m $ "invalid element type:\n" <> toText (weaken elemType')
    m :< WeakTermArrayAccess subject elemType array index -> do
      subject' <- elaborate' ctx subject
      elemType' <- elaborate' ctx elemType
      array' <- elaborate' ctx array
      index' <- elaborate' ctx index
      case elemType' of
        _ :< TermPrim (Prim.Type (PrimNumInt size)) ->
          return $ m :< TermArrayAccess subject' (PrimNumInt size) array' index'
        _ :< TermPrim (Prim.Type (PrimNumFloat size)) ->
          return $ m :< TermArrayAccess subject' (PrimNumFloat size) array' index'
        _ ->
          raiseError ctx m $ "invalid element type:\n" <> toText (weaken elemType')
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
    m :< WeakTermResourceType name ->
      return $ m :< TermResourceType name

-- for now
elaboratePatternList ::
  Context ->
  Hint ->
  [DD.DefiniteDescription] ->
  [(PatternF WeakTerm, WeakTerm)] ->
  IO [(PatternF Term, Term)]
elaboratePatternList ctx m bs patList = do
  patList' <- forM patList $ \((mPat, c, arity, xts), body) -> do
    xts' <- mapM (elaborateWeakBinder ctx) xts
    body' <- elaborate' ctx body
    return ((mPat, c, arity, xts'), body')
  checkCaseSanity ctx m bs patList'
  return patList'

checkCaseSanity :: Context -> Hint -> [DD.DefiniteDescription] -> [(PatternF Term, Term)] -> IO ()
checkCaseSanity ctx m bs patList =
  case (bs, patList) of
    ([], []) ->
      return ()
    (b : bsRest, ((mPat, b', _, _), _) : patListRest) -> do
      if b /= b'
        then
          raiseError ctx mPat $
            "the constructor here is supposed to be `" <> DD.reify b <> "`, but is: `" <> DD.reify b' <> "`"
        else checkCaseSanity ctx m bsRest patListRest
    (b : _, []) ->
      raiseError ctx m $
        "found a non-exhaustive pattern; the clause for `" <> DD.reify b <> "` is missing"
    ([], ((mPat, b, _, _), _) : _) ->
      raiseError ctx mPat $
        "found a redundant pattern; this clause for `" <> DD.reify b <> "` is redundant"

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
    raiseError ctx m "this switch is ill-constructed in that it is not exhaustive"

lookupEnumSet :: Context -> Hint -> ET.EnumTypeName -> IO [EV.EnumValueName]
lookupEnumSet ctx m enumTypeName = do
  let name = ET.reify enumTypeName
  mEnumItems <- lookupGlobal ctx name
  case mEnumItems of
    Just (GN.EnumType enumItems) ->
      return $ map fst enumItems
    _ ->
      raiseError ctx m $ "no such enum defined: " <> DD.reify name

doesContainDefaultCase :: [EnumCase] -> Bool
doesContainDefaultCase enumCaseList =
  case enumCaseList of
    [] ->
      False
    (_ :< EnumCaseDefault) : _ ->
      True
    _ : rest ->
      doesContainDefaultCase rest

-- cs <- readIORef constraintEnv
-- p "==========================================================="
-- forM_ cs $ \(e1, e2) -> do
--   p $ T.unpack $ toText e1
--   p $ T.unpack $ toText e2
--   p "---------------------"
