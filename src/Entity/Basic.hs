{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Entity.Basic where

import Control.Comonad.Cofree
import Data.Binary
import Data.Functor.Classes
import qualified Data.Text as T
import GHC.Generics
import Path
import System.Environment

newtype Ident
  = I (T.Text, Int)
  deriving (Eq, Ord, Generic)

instance Show Ident where
  show (I (s, i)) =
    T.unpack s ++ "-" ++ show i

instance Binary Ident

type Line =
  Int

type Column =
  Int

type Loc =
  (Line, Column)

data Hint = Hint
  { metaFileName :: FilePath,
    metaLocation :: Loc
  }
  deriving (Generic)

instance Binary Hint

type PosInfo =
  (FilePath, Loc)

instance Show Hint where
  show _ =
    "_"

instance Eq Hint where
  _ == _ = True

data EnumCaseF a
  = EnumCaseLabel T.Text
  | EnumCaseInt Int
  | EnumCaseDefault
  deriving (Show, Eq, Ord, Generic)

instance Functor EnumCaseF where
  fmap _ v =
    case v of
      EnumCaseLabel label ->
        EnumCaseLabel label
      EnumCaseInt i ->
        EnumCaseInt i
      EnumCaseDefault ->
        EnumCaseDefault

instance Eq1 EnumCaseF where
  liftEq _ v1 v2 =
    case (v1, v2) of
      (EnumCaseLabel l1, EnumCaseLabel l2)
        | l1 == l2 ->
          True
      (EnumCaseInt i1, EnumCaseInt i2)
        | i1 == i2 ->
          True
      (EnumCaseDefault, EnumCaseDefault) ->
        False
      _ ->
        False

instance Show1 EnumCaseF where
  liftShowsPrec _ _ _ someValue =
    case someValue of
      EnumCaseLabel label ->
        showString $ T.unpack label
      EnumCaseInt i ->
        showString $ show i
      EnumCaseDefault ->
        showString "default"

instance (Binary a) => Binary (EnumCaseF a)

type EnumCase =
  Cofree EnumCaseF Hint

type CompEnumCase =
  Cofree EnumCaseF ()

instance Binary EnumCase

data Opacity
  = OpacityOpaque
  | OpacityTransparent
  deriving (Show, Eq, Generic)

instance Binary Opacity

type BinderF a =
  (Hint, Ident, a)

type PatternF a =
  (Hint, T.Text, [BinderF a])

type DataName =
  T.Text

type ConsName =
  T.Text

type ConsNumber =
  Integer

data LamKindF a
  = LamKindNormal
  | LamKindCons DataName ConsName ConsNumber a
  | LamKindFix (BinderF a)
  deriving (Show, Generic)

instance (Binary a) => Binary (LamKindF a)

isOpaque :: Opacity -> Bool
isOpaque o =
  case o of
    OpacityOpaque ->
      True
    _ ->
      False

fromLamKind :: LamKindF a -> Maybe (Hint, Ident, a)
fromLamKind k =
  case k of
    LamKindFix x ->
      Just x
    _ ->
      Nothing

asText :: Ident -> T.Text
asText (I (s, _)) =
  s

{-# INLINE asText' #-}
asText' :: Ident -> T.Text
asText' (I (s, i)) =
  s <> "-" <> T.pack (show i)

asText'' :: Ident -> T.Text
asText'' (I (_, i)) =
  "_" <> T.pack (show i)

asIdent :: T.Text -> Ident
asIdent s =
  I (s, 0)

asInt :: Ident -> Int
asInt (I (_, i)) =
  i

showHint :: Hint -> String
showHint m = do
  let name = metaFileName m
  let (l, c) = metaLocation m
  name ++ ":" ++ show l ++ ":" ++ show c

newHint :: Int -> Int -> FilePath -> Hint
newHint l c path =
  Hint
    { metaFileName = path,
      metaLocation = (l, c)
    }

getPosInfo :: Hint -> PosInfo
getPosInfo m =
  (metaFileName m, metaLocation m)

showPosInfo :: FilePath -> Loc -> String
showPosInfo path (l, c) =
  path ++ ":" ++ show l ++ ":" ++ show c

getExecPath :: IO (Path Abs File)
getExecPath =
  getExecutablePath >>= parseAbsFile

type Alias =
  T.Text

newtype URL
  = URL T.Text
  deriving (Show)

newtype Checksum
  = Checksum T.Text
  deriving (Show, Ord, Eq)

showChecksum :: Checksum -> T.Text
showChecksum (Checksum checksum) =
  checksum

data AliasInfo
  = AliasInfoUse T.Text
  | AliasInfoPrefix Hint T.Text T.Text
  deriving (Show)

data OutputKind
  = OutputKindObject
  | OutputKindLLVM
  | OutputKindExecutable
  | OutputKindAsm
  deriving (Show)

instance Read OutputKind where
  readsPrec _ "object" =
    [(OutputKindObject, [])]
  readsPrec _ "llvm" =
    [(OutputKindLLVM, [])]
  readsPrec _ "asm" =
    [(OutputKindAsm, [])]
  readsPrec _ _ =
    []
