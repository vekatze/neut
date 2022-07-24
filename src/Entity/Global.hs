module Entity.Global where

import qualified Data.HashMap.Strict as Map
import Data.IORef
import qualified Data.Set as S
import qualified Entity.DeclarationName as DN
import qualified Entity.DefiniteDescription as DD
import Entity.Ident
import Entity.LowComp
import Entity.LowType
import System.IO.Unsafe

--
-- global variables
--

{-# NOINLINE lowDefEnvRef #-}
lowDefEnvRef :: IORef (Map.HashMap DD.DefiniteDescription ([Ident], LowComp))
lowDefEnvRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE lowDeclEnvRef #-}
lowDeclEnvRef :: IORef (Map.HashMap DN.DeclarationName ([LowType], LowType))
lowDeclEnvRef =
  unsafePerformIO $ newIORef initialLowDeclEnv

{-# NOINLINE lowNameSetRef #-}
lowNameSetRef :: IORef (S.Set DD.DefiniteDescription)
lowNameSetRef =
  unsafePerformIO (newIORef S.empty)

{-# NOINLINE nopFreeSetRef #-}
nopFreeSetRef :: IORef (S.Set Int)
nopFreeSetRef =
  unsafePerformIO (newIORef S.empty)

initialLowDeclEnv :: Map.HashMap DN.DeclarationName ([LowType], LowType)
initialLowDeclEnv =
  Map.fromList
    [ (DN.malloc, ([voidPtr], voidPtr)),
      (DN.free, ([voidPtr], voidPtr))
    ]

-- for debug
p :: String -> IO ()
p =
  putStrLn

p' :: (Show a) => a -> IO ()
p' =
  print
