module Context.Elaborate
  ( initialize,
    initializeInferenceEnv,
    insConstraintEnv,
    getConstraintEnv,
    insWeakTypeEnv,
    lookupWeakTypeEnv,
    lookupHoleEnv,
    insHoleEnv,
    insertSubst,
    setConstraintQueue,
    insertConstraint,
    getConstraintQueue,
    getHoleSubst,
    setHoleSubst,
  )
where

import Context.App
import Context.App.Internal
import Context.Throw qualified as Throw
import Data.IntMap qualified as IntMap
import Data.PQueue.Min qualified as Q
import Entity.Constraint qualified as C
import Entity.Hint
import Entity.HoleID qualified as HID
import Entity.HoleSubst qualified as HS
import Entity.Ident
import Entity.Ident.Reify qualified as Ident
import Entity.WeakTerm
import Entity.WeakTerm qualified as WT

initialize :: App ()
initialize = do
  initializeInferenceEnv
  writeRef' weakTypeEnv IntMap.empty

initializeInferenceEnv :: App ()
initializeInferenceEnv = do
  writeRef' constraintEnv []
  writeRef' holeEnv IntMap.empty

insConstraintEnv :: WeakTerm -> WeakTerm -> App ()
insConstraintEnv e1 e2 =
  modifyRef' constraintEnv $ (:) (e1, e2)

getConstraintEnv :: App [(WeakTerm, WeakTerm)]
getConstraintEnv =
  readRef' constraintEnv

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
        Ident.toText' k <> " is not found in the weak type environment."

lookupHoleEnv :: Int -> App (Maybe (WeakTerm, WeakTerm))
lookupHoleEnv i =
  IntMap.lookup i <$> readRef' holeEnv

insHoleEnv :: Int -> WeakTerm -> WeakTerm -> App ()
insHoleEnv i e1 e2 =
  modifyRef' holeEnv $ IntMap.insert i (e1, e2)

insertSubst :: HID.HoleID -> [Ident] -> WT.WeakTerm -> App ()
insertSubst holeID xs e =
  modifyRef' holeSubst $ HS.insert holeID xs e

setConstraintQueue :: Q.MinQueue C.SuspendedConstraint -> App ()
setConstraintQueue =
  writeRef' constraintQueue

insertConstraint :: C.SuspendedConstraint -> App ()
insertConstraint sc =
  modifyRef' constraintQueue $ Q.insert sc

getConstraintQueue :: App C.SuspendedConstraintQueue
getConstraintQueue =
  readRef' constraintQueue

getHoleSubst :: App HS.HoleSubst
getHoleSubst =
  readRef' holeSubst

setHoleSubst :: HS.HoleSubst -> App ()
setHoleSubst =
  writeRef' holeSubst
