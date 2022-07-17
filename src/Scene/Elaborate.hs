module Scene.Elaborate
  ( elaborate,
  )
where

import qualified Context.App as App
import qualified Context.Gensym as Gensym
import qualified Context.Global as Global
import qualified Context.Locator as Locator
import qualified Context.Log as Log
import qualified Context.Throw as Throw
import qualified Context.Type as Type
import Control.Comonad.Cofree
import Control.Monad
import qualified Data.HashMap.Strict as Map
import Data.IORef
import qualified Data.IntMap as IntMap
import Data.List
import qualified Data.Text as T
import Entity.Binder
import qualified Entity.DefiniteDescription as DD
import Entity.EnumCase
import Entity.EnumInfo
import qualified Entity.EnumTypeName as ET
import qualified Entity.EnumValueName as EV
import Entity.Global
import qualified Entity.GlobalName as GN
import Entity.Hint
import qualified Entity.HoleSubst as HS
import qualified Entity.Ident.Reify as Ident
import Entity.LamKind
import Entity.Opacity
import Entity.Pattern
import qualified Entity.Prim as Prim
import Entity.PrimNum
import Entity.Source
import Entity.Stmt
import Entity.Term
import qualified Entity.Term.Reduce as Term
import Entity.Term.Weaken
import Entity.WeakTerm
import qualified Entity.WeakTerm.Subst as WeakTerm
import Entity.WeakTerm.ToText
import Scene.Elaborate.Infer
import Scene.Elaborate.Unify
import Prelude hiding (log)

elaborate :: App.Context -> Source -> Either [Stmt] ([WeakStmt], [EnumInfo]) -> IO [Stmt]
elaborate ctx source cacheOrStmt = do
  ctx' <- specialize ctx
  case cacheOrStmt of
    Left cache -> do
      forM_ cache $ registerTopLevelDef ctx'
      return cache
    Right (defList, enumInfoList) -> do
      mMainDefiniteDescription <- Locator.getMainDefiniteDescription (App.locator ctx) source
      defList' <- mapM (setupDef ctx') defList
      defList'' <- mapM (inferStmt ctx' mMainDefiniteDescription) defList'
      constraintList <- readIORef $ constraintListRef ctx'
      unify (App.gensym ctx) constraintList
      defList''' <- elaborateStmtList ctx defList''
      saveCache (source, defList''') enumInfoList
      return defList'''

registerTopLevelDef :: Context -> Stmt -> IO ()
registerTopLevelDef ctx stmt = do
  case stmt of
    StmtDefine opacity m x impArgNum xts codType e -> do
      modifyIORef' (impArgEnvRef ctx) $ Map.insert x impArgNum
      Type.insert (App.asTypeCtx (base ctx)) x $ weaken $ m :< TermPi xts codType
      unless (isOpaque opacity) $
        modifyIORef' termDefEnvRef $ Map.insert x (opacity, map weakenBinder xts, weaken e)
    StmtDefineResource m name _ _ ->
      Type.insert (App.asTypeCtx (base ctx)) name $ m :< WeakTermTau

setupDef :: Context -> WeakStmt -> IO WeakStmt
setupDef ctx def =
  case def of
    WeakStmtDefine opacity m f impArgNum xts codType e -> do
      Type.insert (App.asTypeCtx (base ctx)) f $ m :< WeakTermPi xts codType
      modifyIORef' (impArgEnvRef ctx) $ Map.insert f impArgNum
      modifyIORef' termDefEnvRef $ Map.insert f (opacity, xts, e)
      return $ WeakStmtDefine opacity m f impArgNum xts codType e
    WeakStmtDefineResource m name discarder copier -> do
      Type.insert (App.asTypeCtx (base ctx)) name $ m :< WeakTermTau
      return $ WeakStmtDefineResource m name discarder copier

inferStmt :: Context -> Maybe DD.DefiniteDescription -> WeakStmt -> IO WeakStmt
inferStmt ctx mMainDD stmt = do
  case stmt of
    WeakStmtDefine isReducible m x impArgNum xts codType e -> do
      (xts', e', codType') <- inferStmtDefine ctx xts e codType
      when (Just x == mMainDD) $
        insConstraintEnv ctx (m :< WeakTermPi [] (i64 m)) (m :< WeakTermPi xts codType)
      return $ WeakStmtDefine isReducible m x impArgNum xts' codType' e'
    WeakStmtDefineResource m name discarder copier ->
      inferDefineResource ctx m name discarder copier

inferDefineResource :: Context -> Hint -> DD.DefiniteDescription -> WeakTerm -> WeakTerm -> IO WeakStmt
inferDefineResource ctx m name discarder copier = do
  (discarder', td) <- infer ctx discarder
  (copier', tc) <- infer ctx copier
  x <- Gensym.newIdentFromText (App.gensym (base ctx)) "_"
  let botTop = m :< WeakTermPi [(m, x, m :< WeakTermEnum constBottom)] (m :< WeakTermEnum constTop)
  let botBot = m :< WeakTermPi [(m, x, m :< WeakTermEnum constBottom)] (m :< WeakTermEnum constBottom)
  insConstraintEnv ctx botTop td
  insConstraintEnv ctx botBot tc
  return $ WeakStmtDefineResource m name discarder' copier'

inferStmtDefine ::
  Context ->
  [BinderF WeakTerm] ->
  WeakTerm ->
  WeakTerm ->
  IO ([BinderF WeakTerm], WeakTerm, WeakTerm)
inferStmtDefine ctx xts e codType = do
  (xts', (e', te)) <- inferBinder ctx [] xts e
  codType' <- inferType ctx codType
  insConstraintEnv ctx codType' te
  return (xts', e', codType')

elaborateStmtList :: App.Context -> [WeakStmt] -> IO [Stmt]
elaborateStmtList ctx stmtList = do
  case stmtList of
    [] ->
      return []
    WeakStmtDefine opacity m x impArgNum xts codType e : rest -> do
      e' <- elaborate' ctx e
      xts' <- mapM (elaborateWeakBinder ctx) xts
      codType' <- elaborate' ctx codType >>= Term.reduce (App.gensym ctx)
      Type.insert (App.asTypeCtx ctx) x $ weaken $ m :< TermPi xts' codType'
      modifyIORef' termDefEnvRef $ Map.insert x (opacity, map weakenBinder xts', weaken e')
      rest' <- elaborateStmtList ctx rest
      return $ StmtDefine opacity m x impArgNum xts' codType' e' : rest'
    WeakStmtDefineResource m name discarder copier : rest -> do
      discarder' <- elaborate' ctx discarder
      copier' <- elaborate' ctx copier
      rest' <- elaborateStmtList ctx rest
      return $ StmtDefineResource m name discarder' copier' : rest'

elaborate' :: App.Context -> WeakTerm -> IO Term
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
      subst <- readIORef substRef
      case HS.lookup h subst of
        Nothing ->
          Throw.raiseError (App.throw ctx) m "couldn't instantiate the hole here"
        Just (xs, e)
          | length xs == length es -> do
            let s = IntMap.fromList $ zip (map Ident.toInt xs) es
            WeakTerm.subst (App.gensym ctx) s e >>= elaborate' ctx
          | otherwise ->
            Throw.raiseError (App.throw ctx) m "arity mismatch"
    m :< WeakTermPrim x ->
      return $ m :< TermPrim x
    m :< WeakTermInt t x -> do
      t' <- elaborate' ctx t >>= Term.reduce (App.gensym ctx)
      case t' of
        _ :< TermPrim (Prim.Type (PrimNumInt size)) ->
          return $ m :< TermInt size x
        _ -> do
          Throw.raiseError (App.throw ctx) m $
            "the term `"
              <> T.pack (show x)
              <> "` is an integer, but its type is: "
              <> toText (weaken t')
    m :< WeakTermFloat t x -> do
      t' <- elaborate' ctx t >>= Term.reduce (App.gensym ctx)
      case t' of
        _ :< TermPrim (Prim.Type (PrimNumFloat size)) ->
          return $ m :< TermFloat size x
        _ ->
          Throw.raiseError (App.throw ctx) m $
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
      t' <- elaborate' ctx t >>= Term.reduce (App.gensym ctx)
      case t' of
        _ :< TermEnum x -> do
          checkSwitchExaustiveness ctx m x ls
          return $ m :< TermEnumElim (e', t') (zip ls es')
        _ ->
          Throw.raiseError (App.throw ctx) m $
            "the type of `"
              <> toText (weaken e')
              <> "` must be an enum type, but is:\n"
              <> toText (weaken t')
    m :< WeakTermQuestion e t -> do
      e' <- elaborate' ctx e
      t' <- elaborate' ctx t
      Log.printNote (App.log ctx) m $ toText (weaken t')
      return e'
    m :< WeakTermMagic der -> do
      der' <- mapM (elaborate' ctx) der
      return $ m :< TermMagic der'
    m :< WeakTermMatch mSubject (e, t) patList -> do
      mSubject' <- mapM (elaborate' ctx) mSubject
      e' <- elaborate' ctx e
      t' <- elaborate' ctx t >>= Term.reduce (App.gensym ctx)
      case t' of
        _ :< TermPiElim (_ :< TermVarGlobal name) _ -> do
          mConsInfoList <- Global.lookup (App.global ctx) name
          case mConsInfoList of
            Just (GN.Data consInfoList) -> do
              patList' <- elaboratePatternList ctx m consInfoList patList
              return $ m :< TermMatch mSubject' (e', t') patList'
            _ ->
              Throw.raiseError (App.throw ctx) (metaOf t) $
                "the type of this term must be a data-type, but its type is:\n" <> toText (weaken t')
        _ -> do
          Throw.raiseError (App.throw ctx) (metaOf t) $
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
          Throw.raiseError (App.throw ctx) m $
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
          Throw.raiseError (App.throw ctx) m $ "invalid element type:\n" <> toText (weaken elemType')
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
          Throw.raiseError (App.throw ctx) m $ "invalid element type:\n" <> toText (weaken elemType')
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
  App.Context ->
  Hint ->
  [DD.DefiniteDescription] ->
  [(PatternF WeakTerm, WeakTerm)] ->
  IO [(PatternF Term, Term)]
elaboratePatternList ctx m bs patList = do
  patList' <- forM patList $ \((mPat, c, xts), body) -> do
    xts' <- mapM (elaborateWeakBinder ctx) xts
    body' <- elaborate' ctx body
    return ((mPat, c, xts'), body')
  checkCaseSanity ctx m bs patList'
  return patList'

checkCaseSanity :: App.Context -> Hint -> [DD.DefiniteDescription] -> [(PatternF Term, Term)] -> IO ()
checkCaseSanity ctx m bs patList =
  case (bs, patList) of
    ([], []) ->
      return ()
    (b : bsRest, ((mPat, b', _), _) : patListRest) -> do
      if b /= b'
        then
          Throw.raiseError (App.throw ctx) mPat $
            "the constructor here is supposed to be `" <> DD.reify b <> "`, but is: `" <> DD.reify b' <> "`"
        else checkCaseSanity ctx m bsRest patListRest
    (b : _, []) ->
      Throw.raiseError (App.throw ctx) m $
        "found a non-exhaustive pattern; the clause for `" <> DD.reify b <> "` is missing"
    ([], ((mPat, b, _), _) : _) ->
      Throw.raiseError (App.throw ctx) mPat $
        "found a redundant pattern; this clause for `" <> DD.reify b <> "` is redundant"

elaborateWeakBinder :: App.Context -> BinderF WeakTerm -> IO (BinderF Term)
elaborateWeakBinder ctx (m, x, t) = do
  t' <- elaborate' ctx t
  return (m, x, t')

elaborateKind :: App.Context -> LamKindF WeakTerm -> IO (LamKindF Term)
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

checkSwitchExaustiveness :: App.Context -> Hint -> ET.EnumTypeName -> [EnumCase] -> IO ()
checkSwitchExaustiveness ctx m enumTypeName caseList = do
  let containsDefaultCase = doesContainDefaultCase caseList
  enumSet <- lookupEnumSet ctx m enumTypeName
  let len = toInteger $ length (nub caseList)
  unless (toInteger (length enumSet) <= len || containsDefaultCase) $
    Throw.raiseError (App.throw ctx) m "this switch is ill-constructed in that it is not exhaustive"

lookupEnumSet :: App.Context -> Hint -> ET.EnumTypeName -> IO [EV.EnumValueName]
lookupEnumSet ctx m enumTypeName = do
  let name = ET.reify enumTypeName
  mEnumItems <- Global.lookup (App.global ctx) name
  case mEnumItems of
    Just (GN.EnumType enumItems) ->
      return $ map fst enumItems
    _ ->
      Throw.raiseError (App.throw ctx) m $ "no such enum defined: " <> DD.reify name

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
