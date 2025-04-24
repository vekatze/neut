module Move.Context.Elaborate
  ( initialize,
    initializeInferenceEnv,
    getConstraintEnv,
    setSuspendedEnv,
    getSuspendedEnv,
    lookupWeakTypeEnvMaybe,
    getHoleSubst,
    setHoleSubst,
    fillHole,
  )
where

import Data.IntMap qualified as IntMap
import Data.Text qualified as T
import Move.Context.App
import Move.Context.App.Internal
import Move.Context.EIO (toApp)
import Move.Context.Throw qualified as Throw
import Move.Scene.WeakTerm.Subst qualified as Subst
import Rule.Constraint qualified as C
import Rule.Hint
import Rule.HoleID qualified as HID
import Rule.HoleSubst qualified as HS
import Rule.Ident.Reify qualified as Ident
import Rule.WeakTerm
import Rule.WeakTerm qualified as WT

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

getConstraintEnv :: App [C.Constraint]
getConstraintEnv =
  readRef' constraintEnv

getSuspendedEnv :: App [C.SuspendedConstraint]
getSuspendedEnv =
  readRef' suspendedEnv

setSuspendedEnv :: [C.SuspendedConstraint] -> App ()
setSuspendedEnv =
  writeRef' suspendedEnv

lookupWeakTypeEnvMaybe :: Int -> App (Maybe WeakTerm)
lookupWeakTypeEnvMaybe k = do
  wtenv <- readRef' weakTypeEnv
  return $ IntMap.lookup k wtenv

getHoleSubst :: App HS.HoleSubst
getHoleSubst =
  readRef' holeSubst

setHoleSubst :: HS.HoleSubst -> App ()
setHoleSubst =
  writeRef' holeSubst

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
