module Main.Move.Scene.Elaborate.Handle.Constraint
  ( Handle,
    new,
    get,
    set,
    insert,
    insertActualityConstraint,
    insertIntegerConstraint,
    getSuspendedConstraints,
    setSuspendedConstraints,
  )
where

import Data.IORef
import Main.Rule.Constraint qualified as C
import Main.Rule.WeakTerm qualified as WT

data Handle = Handle
  { constraintEnvRef :: IORef [C.Constraint],
    suspendedEnvRef :: IORef [C.SuspendedConstraint]
  }

new :: IO Handle
new = do
  constraintEnvRef <- newIORef []
  suspendedEnvRef <- newIORef []
  return $ Handle {..}

insert :: Handle -> WT.WeakTerm -> WT.WeakTerm -> IO ()
insert h expected actual = do
  modifyIORef' (constraintEnvRef h) $ (:) (C.Eq expected actual)

insertActualityConstraint :: Handle -> WT.WeakTerm -> IO ()
insertActualityConstraint h t = do
  modifyIORef' (constraintEnvRef h) $ (:) (C.Actual t)

insertIntegerConstraint :: Handle -> WT.WeakTerm -> IO ()
insertIntegerConstraint h t = do
  modifyIORef' (constraintEnvRef h) $ (:) (C.Integer t)

get :: Handle -> IO [C.Constraint]
get h = do
  readIORef (constraintEnvRef h)

set :: Handle -> [C.Constraint] -> IO ()
set h cs = do
  writeIORef (constraintEnvRef h) cs

getSuspendedConstraints :: Handle -> IO [C.SuspendedConstraint]
getSuspendedConstraints h = do
  readIORef (suspendedEnvRef h)

setSuspendedConstraints :: Handle -> [C.SuspendedConstraint] -> IO ()
setSuspendedConstraints h cs = do
  writeIORef (suspendedEnvRef h) cs
