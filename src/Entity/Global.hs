module Entity.Global where

import qualified Data.HashMap.Strict as Map
import Data.IORef
import qualified Data.PQueue.Min as Q
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.Binder
import Entity.Comp
import Entity.Constraint
import qualified Entity.DefiniteDescription as DD
import qualified Entity.HoleSubst as HS
import Entity.Ident
import Entity.LowComp
import Entity.LowType
import Entity.Opacity
import Entity.WeakTerm
import System.IO.Unsafe

--
-- global variables
--

{-# NOINLINE suspendedConstraintQueueRef #-}
suspendedConstraintQueueRef :: IORef SuspendedConstraintQueue
suspendedConstraintQueueRef =
  unsafePerformIO (newIORef Q.empty)

{-# NOINLINE substRef #-}
substRef :: IORef HS.HoleSubst
substRef =
  unsafePerformIO (newIORef HS.empty)

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
