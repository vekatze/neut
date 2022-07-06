{-# LANGUAGE TemplateHaskell #-}

module Entity.Global where

import qualified Data.HashMap.Lazy as Map
import Data.IORef
import qualified Data.IntMap as IntMap
import qualified Data.PQueue.Min as Q
import qualified Data.Set as S
import qualified Data.Text as T
import Entity.Binder
import Entity.Comp
import Entity.Constraint
import Entity.Ident
import Entity.LowComp
import Entity.LowType
import Entity.Opacity
import Entity.WeakTerm
import Path
import Path.IO
import System.IO.Unsafe

--
-- global variables
--

{-# NOINLINE dataEnvRef #-}
dataEnvRef :: IORef (Map.HashMap T.Text [T.Text])
dataEnvRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE constructorEnvRef #-}
constructorEnvRef :: IORef (Map.HashMap T.Text Int)
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
impArgEnvRef :: IORef (Map.HashMap T.Text Int)
impArgEnvRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE substRef #-}
substRef :: IORef (IntMap.IntMap WeakTerm)
substRef =
  unsafePerformIO (newIORef IntMap.empty)

{-# NOINLINE termTypeEnvRef #-}
termTypeEnvRef :: IORef (Map.HashMap T.Text WeakTerm)
termTypeEnvRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE termDefEnvRef #-}
termDefEnvRef :: IORef (Map.HashMap T.Text (Opacity, [BinderF WeakTerm], WeakTerm))
termDefEnvRef =
  unsafePerformIO (newIORef Map.empty)

{-# NOINLINE resourceTypeSetRef #-}
resourceTypeSetRef :: IORef (S.Set T.Text)
resourceTypeSetRef =
  unsafePerformIO $ newIORef S.empty

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

-- {-# NOINLINE hasCacheSetRef #-}
-- hasCacheSetRef :: IORef (S.Set (Path Abs File))
-- hasCacheSetRef =
--   unsafePerformIO (newIORef S.empty)

-- {-# NOINLINE hasObjectSetRef #-}
-- hasObjectSetRef :: IORef (S.Set (Path Abs File))
-- hasObjectSetRef =
--   unsafePerformIO (newIORef S.empty)

sourceFileExtension :: T.Text
sourceFileExtension =
  "neut"

{-# INLINE nsSep #-}
nsSep :: T.Text
nsSep =
  "."

definiteSep :: T.Text
definiteSep =
  "::"

constBottom :: T.Text
constBottom =
  "bottom"

constTop :: T.Text
constTop =
  "top"

constTopUnit :: T.Text
constTopUnit =
  "unit"

{-# INLINE constBool #-}
constBool :: T.Text
constBool =
  "bool"

{-# INLINE constBoolTrue #-}
constBoolTrue :: T.Text
constBoolTrue =
  "true"

{-# INLINE constBoolFalse #-}
constBoolFalse :: T.Text
constBoolFalse =
  "false"

unsafePtr :: T.Text
unsafePtr =
  "unsafe-pointer"

initialLowDeclEnv :: Map.HashMap T.Text ([LowType], LowType)
initialLowDeclEnv =
  Map.fromList
    [ ("malloc", ([voidPtr], voidPtr)),
      ("free", ([voidPtr], voidPtr))
    ]

cartImmName :: T.Text
cartImmName =
  "imm"

cartClsName :: T.Text
cartClsName =
  "cls"

cartCellName :: T.Text
cartCellName =
  "cell"

getCacheDirPath :: IO (Path Abs Dir)
getCacheDirPath = do
  getXdgDir XdgCache (Just $(mkRelDir "neut")) >>= returnDirectory

getLibraryDirPath :: IO (Path Abs Dir)
getLibraryDirPath = do
  basePath <- getCacheDirPath
  returnDirectory $ basePath </> $(mkRelDir "library")

returnDirectory :: Path Abs Dir -> IO (Path Abs Dir)
returnDirectory path =
  ensureDir path >> return path

-- for debug
p :: String -> IO ()
p =
  putStrLn

p' :: (Show a) => a -> IO ()
p' =
  print
