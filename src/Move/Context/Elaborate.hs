module Move.Context.Elaborate
  ( initialize,
    initializeInferenceEnv,
    insConstraintEnv,
    insertActualityConstraint,
    insertIntegerConstraint,
    setConstraintEnv,
    getConstraintEnv,
    setSuspendedEnv,
    getSuspendedEnv,
    insWeakTypeEnv,
    lookupWeakTypeEnv,
    lookupWeakTypeEnvMaybe,
    lookupHoleEnv,
    insHoleEnv,
    insertSubst,
    newHole,
    newTypeHoleList,
    getHoleSubst,
    setHoleSubst,
    fillHole,
    reduceWeakType,
  )
where

import Control.Comonad.Cofree
import Data.IntMap qualified as IntMap
import Data.Text qualified as T
import Move.Context.App
import Move.Context.App.Internal
import Move.Context.EIO (toApp)
import Move.Context.Gensym qualified as Gensym
import Move.Context.Throw qualified as Throw
import Move.Context.WeakDefinition qualified as WeakDefinition
import Move.Scene.WeakTerm.Reduce qualified as Reduce
import Move.Scene.WeakTerm.Subst qualified as Subst
import Rule.Binder
import Rule.Constraint qualified as C
import Rule.Hint
import Rule.HoleID qualified as HID
import Rule.HoleSubst qualified as HS
import Rule.Ident
import Rule.Ident.Reify qualified as Ident
import Rule.WeakTerm
import Rule.WeakTerm qualified as WT

type BoundVarEnv = [BinderF WT.WeakTerm]

initialize :: App ()
initialize = do
  initializeInferenceEnv
  writeRef' weakTypeEnv IntMap.empty
  setHoleSubst HS.empty

initializeInferenceEnv :: App ()
initializeInferenceEnv = do
  writeRef' constraintEnv []
  writeRef' suspendedEnv []
  writeRef' holeEnv IntMap.empty

insConstraintEnv :: WeakTerm -> WeakTerm -> App ()
insConstraintEnv expected actual = do
  modifyRef' constraintEnv $ (:) (C.Eq expected actual)

insertActualityConstraint :: WeakTerm -> App ()
insertActualityConstraint t = do
  modifyRef' constraintEnv $ (:) (C.Actual t)

insertIntegerConstraint :: WeakTerm -> App ()
insertIntegerConstraint t = do
  modifyRef' constraintEnv $ (:) (C.Integer t)

getConstraintEnv :: App [C.Constraint]
getConstraintEnv =
  readRef' constraintEnv

setConstraintEnv :: [C.Constraint] -> App ()
setConstraintEnv =
  writeRef' constraintEnv

getSuspendedEnv :: App [C.SuspendedConstraint]
getSuspendedEnv =
  readRef' suspendedEnv

setSuspendedEnv :: [C.SuspendedConstraint] -> App ()
setSuspendedEnv =
  writeRef' suspendedEnv

insWeakTypeEnv :: Ident -> WeakTerm -> App ()
insWeakTypeEnv k v =
  modifyRef' weakTypeEnv $ IntMap.insert (Ident.toInt k) v

lookupWeakTypeEnv :: Hint -> Ident -> App WeakTerm
lookupWeakTypeEnv m k = do
  wtenv <- readRef' weakTypeEnv
  case IntMap.lookup (Ident.toInt k) wtenv of
    Just t ->
      return t
    Nothing ->
      Throw.raiseCritical m $
        "`" <> Ident.toText' k <> "` is not found in the weak type environment."

lookupWeakTypeEnvMaybe :: Int -> App (Maybe WeakTerm)
lookupWeakTypeEnvMaybe k = do
  wtenv <- readRef' weakTypeEnv
  return $ IntMap.lookup k wtenv

lookupHoleEnv :: Int -> App (Maybe (WeakTerm, WeakTerm))
lookupHoleEnv i =
  IntMap.lookup i <$> readRef' holeEnv

insHoleEnv :: Int -> WeakTerm -> WeakTerm -> App ()
insHoleEnv i e1 e2 =
  modifyRef' holeEnv $ IntMap.insert i (e1, e2)

insertSubst :: HID.HoleID -> [Ident] -> WT.WeakTerm -> App ()
insertSubst holeID xs e =
  modifyRef' holeSubst $ HS.insert holeID xs e

getHoleSubst :: App HS.HoleSubst
getHoleSubst =
  readRef' holeSubst

setHoleSubst :: HS.HoleSubst -> App ()
setHoleSubst =
  writeRef' holeSubst

newHole :: Hint -> BoundVarEnv -> App WT.WeakTerm
newHole m varEnv = do
  Gensym.newHole m $ map (\(mx, x, _) -> mx :< WT.Var x) varEnv

-- In context varEnv == [x1, ..., xn], `newTypeHoleList varEnv [y1, ..., ym]` generates
-- the following list:
--
--   [(y1,   ?M1   @ (x1, ..., xn)),
--    (y2,   ?M2   @ (x1, ..., xn, y1),
--    ...,
--    (y{m}, ?M{m} @ (x1, ..., xn, y1, ..., y{m-1}))]
--
-- inserting type information `yi : ?Mi @ (x1, ..., xn, y1, ..., y{i-1})
newTypeHoleList :: BoundVarEnv -> [(Ident, Hint)] -> App [BinderF WT.WeakTerm]
newTypeHoleList varEnv ids =
  case ids of
    [] ->
      return []
    ((x, m) : rest) -> do
      t <- newHole m varEnv
      insWeakTypeEnv x t
      ts <- newTypeHoleList ((m, x, t) : varEnv) rest
      return $ (m, x, t) : ts

reduceWeakType :: WT.WeakTerm -> App WT.WeakTerm
reduceWeakType e = do
  e' <- Reduce.reduce e
  case e' of
    m :< WT.Hole hole es ->
      fillHole m hole es >>= reduceWeakType
    m :< WT.PiElim (_ :< WT.VarGlobal _ name) args -> do
      mLam <- WeakDefinition.lookup name
      case mLam of
        Just lam ->
          reduceWeakType $ m :< WT.PiElim lam args
        Nothing -> do
          return e'
    _ ->
      return e'

fillHole ::
  Hint ->
  HID.HoleID ->
  [WT.WeakTerm] ->
  App WT.WeakTerm
fillHole m h es = do
  holeSubst <- getHoleSubst
  substHandle <- Subst.new
  case HS.lookup h holeSubst of
    Nothing ->
      Throw.raiseError m $ "Could not instantiate the hole here: " <> T.pack (show h)
    Just (xs, e)
      | length xs == length es -> do
          let s = IntMap.fromList $ zip (map Ident.toInt xs) (map Right es)
          toApp $ Subst.subst substHandle s e
      | otherwise ->
          Throw.raiseError m "Arity mismatch"
