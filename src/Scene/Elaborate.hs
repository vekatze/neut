module Scene.Elaborate
  ( elaborate,
    Context (..),
  )
where

import qualified Context.Definition as Definition
import qualified Context.Env as Env
import qualified Context.Global as Global
import qualified Context.Implicit as Implicit
import qualified Context.Locator as Locator
import qualified Context.Log as Log
import qualified Context.Throw as Throw
import qualified Context.Type as Type
import Control.Comonad.Cofree
import Control.Monad
import qualified Data.IntMap as IntMap
import Data.List
import qualified Data.Text as T
import Entity.Binder
import qualified Entity.DefiniteDescription as DD
import qualified Entity.EnumCase as EC
import Entity.EnumInfo
import qualified Entity.EnumTypeName as ET
import qualified Entity.EnumValueName as EV
import qualified Entity.GlobalName as GN
import Entity.Hint
import qualified Entity.HoleSubst as HS
import qualified Entity.Ident.Reify as Ident
import qualified Entity.LamKind as LK
import Entity.Pattern
import qualified Entity.Prim as Prim
import qualified Entity.PrimType as PT
import qualified Entity.Source as Source
import Entity.Stmt
import qualified Entity.Term as TM
import qualified Entity.Term.Reduce as Term
import qualified Entity.Term.Subst as Subst
import Entity.Term.Weaken
import qualified Entity.WeakTerm as WT
import qualified Entity.WeakTerm.Subst as WT
import Entity.WeakTerm.ToText
import qualified Scene.Elaborate.Infer as Infer
import qualified Scene.Elaborate.Unify as Unify

class
  ( Infer.Context m,
    Unify.Context m,
    Subst.Context m,
    Log.Context m,
    Locator.Context m,
    Global.Context m,
    Definition.Context m
  ) =>
  Context m
  where
  initialize :: m ()
  saveCache :: Program -> [EnumInfo] -> m ()

elaborate :: Context m => Source.Source -> Either [Stmt] ([WeakStmt], [EnumInfo]) -> m [Stmt]
elaborate source cacheOrStmt = do
  initialize
  case cacheOrStmt of
    Left cache -> do
      forM_ cache registerTopLevelDef
      return cache
    Right (defList, enumInfoList) -> do
      mMainDefiniteDescription <- Locator.getMainDefiniteDescription source
      -- infer
      defList' <- mapM setupDef defList
      defList'' <- mapM (inferStmt mMainDefiniteDescription) defList'
      constraintList <- Env.getConstraintEnv
      -- unify
      Unify.unify constraintList >>= Env.setHoleSubst
      -- elaborate
      defList''' <- elaborateStmtList defList''
      saveCache (source, defList''') enumInfoList
      return defList'''

registerTopLevelDef :: Context m => Stmt -> m ()
registerTopLevelDef stmt = do
  case stmt of
    StmtDefine opacity m x impArgNum xts codType e -> do
      Implicit.insert x impArgNum
      Type.insert x $ weaken $ m :< TM.Pi xts codType
      Definition.insert opacity m x (map weakenBinder xts) (weaken e)

setupDef :: Context m => WeakStmt -> m WeakStmt
setupDef def =
  case def of
    WeakStmtDefine opacity m f impArgNum xts codType e -> do
      Type.insert f $ m :< WT.Pi xts codType
      Implicit.insert f impArgNum
      Definition.insert opacity m f xts e
      return $ WeakStmtDefine opacity m f impArgNum xts codType e

inferStmt :: Infer.Context m => Maybe DD.DefiniteDescription -> WeakStmt -> m WeakStmt
inferStmt mMainDD stmt = do
  case stmt of
    WeakStmtDefine isReducible m x impArgNum xts codType e -> do
      (xts', e', codType') <- inferStmtDefine xts e codType
      when (Just x == mMainDD) $
        Env.insConstraintEnv (m :< WT.Pi [] (WT.i64 m)) (m :< WT.Pi xts codType)
      return $ WeakStmtDefine isReducible m x impArgNum xts' codType' e'

inferStmtDefine ::
  Infer.Context m =>
  [BinderF WT.WeakTerm] ->
  WT.WeakTerm ->
  WT.WeakTerm ->
  m ([BinderF WT.WeakTerm], WT.WeakTerm, WT.WeakTerm)
inferStmtDefine xts e codType = do
  (xts', (e', te)) <- Infer.inferBinder [] xts e
  codType' <- Infer.inferType codType
  Env.insConstraintEnv codType' te
  return (xts', e', codType')

elaborateStmtList :: Context m => [WeakStmt] -> m [Stmt]
elaborateStmtList stmtList = do
  case stmtList of
    [] ->
      return []
    WeakStmtDefine opacity m x impArgNum xts codType e : rest -> do
      e' <- elaborate' e
      xts' <- mapM elaborateWeakBinder xts
      codType' <- elaborate' codType >>= Term.reduce
      Type.insert x $ weaken $ m :< TM.Pi xts' codType'
      Definition.insert opacity m x (map weakenBinder xts') (weaken e')
      rest' <- elaborateStmtList rest
      return $ StmtDefine opacity m x impArgNum xts' codType' e' : rest'

elaborate' :: Context m => WT.WeakTerm -> m TM.Term
elaborate' term =
  case term of
    m :< WT.Tau ->
      return $ m :< TM.Tau
    m :< WT.Var x ->
      return $ m :< TM.Var x
    m :< WT.VarGlobal name arity ->
      return $ m :< TM.VarGlobal name arity
    m :< WT.Pi xts t -> do
      xts' <- mapM elaborateWeakBinder xts
      t' <- elaborate' t
      return $ m :< TM.Pi xts' t'
    m :< WT.PiIntro kind xts e -> do
      kind' <- elaborateKind kind
      xts' <- mapM elaborateWeakBinder xts
      e' <- elaborate' e
      return $ m :< TM.PiIntro kind' xts' e'
    m :< WT.PiElim e es -> do
      e' <- elaborate' e
      es' <- mapM elaborate' es
      return $ m :< TM.PiElim e' es'
    m :< WT.Sigma xts -> do
      xts' <- mapM elaborateWeakBinder xts
      return $ m :< TM.Sigma xts'
    m :< WT.SigmaIntro es -> do
      es' <- mapM elaborate' es
      return $ m :< TM.SigmaIntro es'
    m :< WT.SigmaElim xts e1 e2 -> do
      e1' <- elaborate' e1
      xts' <- mapM elaborateWeakBinder xts
      e2' <- elaborate' e2
      return $ m :< TM.SigmaElim xts' e1' e2'
    m :< WT.Let mxt e1 e2 -> do
      e1' <- elaborate' e1
      mxt' <- elaborateWeakBinder mxt
      e2' <- elaborate' e2
      return $ m :< TM.Let mxt' e1' e2'
    m :< WT.Aster h es -> do
      holeSubst <- Env.getHoleSubst
      case HS.lookup h holeSubst of
        Nothing ->
          Throw.raiseError m "couldn't instantiate the hole here"
        Just (xs, e)
          | length xs == length es -> do
              let s = IntMap.fromList $ zip (map Ident.toInt xs) es
              WT.subst s e >>= elaborate'
          | otherwise ->
              Throw.raiseError m "arity mismatch"
    m :< WT.Prim x ->
      return $ m :< TM.Prim x
    m :< WT.Int t x -> do
      t' <- elaborate' t >>= Term.reduce
      case t' of
        _ :< TM.Prim (Prim.Type (PT.Int size)) ->
          return $ m :< TM.Int size x
        _ -> do
          Throw.raiseError m $
            "the term `"
              <> T.pack (show x)
              <> "` is an integer, but its type is: "
              <> toText (weaken t')
    m :< WT.Float t x -> do
      t' <- elaborate' t >>= Term.reduce
      case t' of
        _ :< TM.Prim (Prim.Type (PT.Float size)) ->
          return $ m :< TM.Float size x
        _ ->
          Throw.raiseError m $
            "the term `"
              <> T.pack (show x)
              <> "` is a float, but its type is:\n"
              <> toText (weaken t')
    m :< WT.Enum k ->
      return $ m :< TM.Enum k
    m :< WT.EnumIntro label ->
      return $ m :< TM.EnumIntro label
    m :< WT.EnumElim (e, t) les -> do
      e' <- elaborate' e
      let (ls, es) = unzip les
      es' <- mapM elaborate' es
      t' <- elaborate' t >>= Term.reduce
      case t' of
        _ :< TM.Enum x -> do
          checkSwitchExaustiveness m x ls
          return $ m :< TM.EnumElim (e', t') (zip ls es')
        _ ->
          Throw.raiseError m $
            "the type of `"
              <> toText (weaken e')
              <> "` must be an enum type, but is:\n"
              <> toText (weaken t')
    m :< WT.Question e t -> do
      e' <- elaborate' e
      t' <- elaborate' t
      Log.printNote m $ toText (weaken t')
      return e'
    m :< WT.Magic der -> do
      der' <- mapM elaborate' der
      return $ m :< TM.Magic der'
    m :< WT.Match (e, t) patList -> do
      e' <- elaborate' e
      t' <- elaborate' t >>= Term.reduce
      case t' of
        _ :< TM.PiElim (_ :< TM.VarGlobal name _) _ -> do
          mConsInfoList <- Global.lookup name
          case mConsInfoList of
            Just (GN.Data _ consInfoList) -> do
              patList' <- elaboratePatternList m consInfoList patList
              return $ m :< TM.Match (e', t') patList'
            _ ->
              Throw.raiseError (WT.metaOf t) $
                "the type of this term must be a data-type, but its type is:\n" <> toText (weaken t')
        _ -> do
          Throw.raiseError (WT.metaOf t) $
            "the type of this term must be a data-type, but its type is:\n" <> toText (weaken t')

-- for now
elaboratePatternList ::
  Context m =>
  Hint ->
  [DD.DefiniteDescription] ->
  [(PatternF WT.WeakTerm, WT.WeakTerm)] ->
  m [(PatternF TM.Term, TM.Term)]
elaboratePatternList m bs patList = do
  patList' <- forM patList $ \((mPat, c, arity, xts), body) -> do
    xts' <- mapM elaborateWeakBinder xts
    body' <- elaborate' body
    return ((mPat, c, arity, xts'), body')
  checkCaseSanity m bs patList'
  return patList'

checkCaseSanity :: Context m => Hint -> [DD.DefiniteDescription] -> [(PatternF TM.Term, TM.Term)] -> m ()
checkCaseSanity m bs patList =
  case (bs, patList) of
    ([], []) ->
      return ()
    (b : bsRest, ((mPat, b', _, _), _) : patListRest) -> do
      if b /= b'
        then
          Throw.raiseError mPat $
            "the constructor here is supposed to be `" <> DD.reify b <> "`, but is: `" <> DD.reify b' <> "`"
        else checkCaseSanity m bsRest patListRest
    (b : _, []) ->
      Throw.raiseError m $
        "found a non-exhaustive pattern; the clause for `" <> DD.reify b <> "` is missing"
    ([], ((mPat, b, _, _), _) : _) ->
      Throw.raiseError mPat $
        "found a redundant pattern; this clause for `" <> DD.reify b <> "` is redundant"

elaborateWeakBinder :: Context m => BinderF WT.WeakTerm -> m (BinderF TM.Term)
elaborateWeakBinder (m, x, t) = do
  t' <- elaborate' t
  return (m, x, t')

elaborateKind :: Context m => LK.LamKindF WT.WeakTerm -> m (LK.LamKindF TM.Term)
elaborateKind kind =
  case kind of
    LK.Normal ->
      return LK.Normal
    LK.Cons dataName consName consNumber dataType -> do
      dataType' <- elaborate' dataType
      return $ LK.Cons dataName consName consNumber dataType'
    LK.Fix xt -> do
      xt' <- elaborateWeakBinder xt
      return $ LK.Fix xt'

checkSwitchExaustiveness :: Context m => Hint -> ET.EnumTypeName -> [EC.EnumCase] -> m ()
checkSwitchExaustiveness m enumTypeName caseList = do
  let containsDefaultCase = doesContainDefaultCase caseList
  enumSet <- lookupEnumSet m enumTypeName
  let len = toInteger $ length (nub caseList)
  unless (toInteger (length enumSet) <= len || containsDefaultCase) $
    Throw.raiseError m "this switch is ill-constructed in that it is not exhaustive"

lookupEnumSet :: Context m => Hint -> ET.EnumTypeName -> m [EV.EnumValueName]
lookupEnumSet m enumTypeName = do
  let name = ET.reify enumTypeName
  mEnumItems <- Global.lookup name
  case mEnumItems of
    Just (GN.EnumType enumItems) ->
      return $ map fst enumItems
    _ ->
      Throw.raiseError m $ "no such enum defined: " <> DD.reify name

doesContainDefaultCase :: [EC.EnumCase] -> Bool
doesContainDefaultCase enumCaseList =
  case enumCaseList of
    [] ->
      False
    (_ :< EC.Default) : _ ->
      True
    _ : rest ->
      doesContainDefaultCase rest

-- cs <- readIORef constraintEnv
-- p "==========================================================="
-- forM_ cs $ \(e1, e2) -> do
--   p $ T.unpack $ toText e1
--   p $ T.unpack $ toText e2
--   p "---------------------"
