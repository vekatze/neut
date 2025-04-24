module Move.Context.Elaborate
  ( initialize,
    initializeInferenceEnv,
    getConstraintEnv,
    setSuspendedEnv,
    getSuspendedEnv,
    lookupWeakTypeEnvMaybe,
    getHoleSubst,
    setHoleSubst,
  )
where

import Data.IntMap qualified as IntMap
import Move.Context.App
import Move.Context.App.Internal
import Rule.Constraint qualified as C
import Rule.HoleSubst qualified as HS
import Rule.WeakTerm

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
