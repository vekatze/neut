module Entity.Global where

import qualified Data.HashMap.Strict as Map
import Data.IORef
import qualified Data.IntMap as IntMap
import qualified Data.PQueue.Min as Q
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.Binder
import Entity.Comp
import Entity.Constraint
import qualified Entity.DefiniteDescription as DD
import Entity.Ident
import Entity.LowComp
import Entity.LowType
import Entity.Opacity
import Entity.WeakTerm
import System.IO.Unsafe

--
-- global variables
--

{-# NOINLINE dataEnvRef #-}
dataEnvRef :: IORef (Map.HashMap DD.DefiniteDescription [DD.DefiniteDescription])
dataEnvRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE constructorEnvRef #-}
constructorEnvRef :: IORef (Map.HashMap DD.DefiniteDescription Int)
constructorEnvRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE weakTypeEnvRef #-}
weakTypeEnvRef :: IORef (IntMap.IntMap WeakTerm)
weakTypeEnvRef =
  unsafePerformIO (newIORef IntMap.empty)

{-# NOINLINE holeEnvRef #-}
holeEnvRef :: IORef (IntMap.IntMap (WeakTerm, WeakTerm))
holeEnvRef =
  unsafePerformIO (newIORef IntMap.empty)

{-# NOINLINE constraintListRef #-}
constraintListRef :: IORef [Constraint]
constraintListRef =
  unsafePerformIO (newIORef [])

{-# NOINLINE suspendedConstraintQueueRef #-}
suspendedConstraintQueueRef :: IORef SuspendedConstraintQueue
suspendedConstraintQueueRef =
  unsafePerformIO (newIORef Q.empty)

{-# NOINLINE impArgEnvRef #-}
impArgEnvRef :: IORef (Map.HashMap DD.DefiniteDescription Int)
impArgEnvRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE substRef #-}
substRef :: IORef (IntMap.IntMap WeakTerm)
substRef =
  unsafePerformIO (newIORef IntMap.empty)

{-# NOINLINE termTypeEnvRef #-}
termTypeEnvRef :: IORef (Map.HashMap DD.DefiniteDescription WeakTerm)
termTypeEnvRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE termDefEnvRef #-}
termDefEnvRef :: IORef (Map.HashMap DD.DefiniteDescription (Opacity, [BinderF WeakTerm], WeakTerm))
termDefEnvRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE compDefEnvRef #-}
compDefEnvRef :: IORef (Map.HashMap T.Text (Opacity, [Ident], Comp))
compDefEnvRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE lowDefEnvRef #-}
lowDefEnvRef :: IORef (Map.HashMap T.Text ([Ident], LowComp))
lowDefEnvRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE lowDeclEnvRef #-}
lowDeclEnvRef :: IORef (Map.HashMap T.Text ([LowType], LowType))
lowDeclEnvRef =
  unsafePerformIO $ newIORef initialLowDeclEnv

{-# NOINLINE lowNameSetRef #-}
lowNameSetRef :: IORef (S.Set T.Text)
lowNameSetRef =
  unsafePerformIO (newIORef S.empty)

{-# NOINLINE nopFreeSetRef #-}
nopFreeSetRef :: IORef (S.Set Int)
nopFreeSetRef =
  unsafePerformIO (newIORef S.empty)

initialLowDeclEnv :: Map.HashMap T.Text ([LowType], LowType)
initialLowDeclEnv =
  Map.fromList
    [ ("malloc", ([voidPtr], voidPtr)),
      ("free", ([voidPtr], voidPtr))
    ]

-- for debug
p :: String -> IO ()
p =
  putStrLn

p' :: (Show a) => a -> IO ()
p' =
  print
