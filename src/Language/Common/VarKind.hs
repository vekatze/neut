module Language.Common.VarKind
  ( VarKind (..),
    CopyKind (..),
    TypeAttr (..),
    normal,
    exponential,
    defined,
    isExp,
    canCopy,
    withSource,
    withAttr,
    hasAttr,
    attrList,
    attrClosure,
    reifyAttr,
    reifyUnsafeAttr,
  )
where

import Binary.Derive (deriveBinary)
import Data.Set qualified as S
import Data.Text qualified as T

data TypeAttr
  = Sized
  | Actual
  deriving (Eq, Ord, Show, Enum, Bounded)

data CopyKind
  = NoCopy
  | DeclaredCopy
  | DefinedCopy
  deriving (Eq, Ord, Show)

data VarKind = VarKind
  { copyKind :: CopyKind,
    isSource :: Bool,
    typeAttrs :: S.Set TypeAttr
  }
  deriving (Eq, Ord, Show)

normal :: VarKind
normal =
  VarKind {copyKind = NoCopy, isSource = False, typeAttrs = S.empty}

exponential :: VarKind
exponential =
  VarKind {copyKind = DeclaredCopy, isSource = False, typeAttrs = S.empty}

defined :: VarKind
defined =
  VarKind {copyKind = DefinedCopy, isSource = False, typeAttrs = S.empty}

isExp :: VarKind -> Bool
isExp k =
  copyKind k == DeclaredCopy

canCopy :: VarKind -> Bool
canCopy k =
  copyKind k /= NoCopy

withSource :: VarKind -> VarKind
withSource k =
  k {isSource = True}

withAttr :: TypeAttr -> VarKind -> VarKind
withAttr attr k =
  k {typeAttrs = S.insert attr (typeAttrs k)}

hasAttr :: TypeAttr -> VarKind -> Bool
hasAttr attr k =
  S.member attr (typeAttrs k)

attrList :: VarKind -> [TypeAttr]
attrList k =
  S.toAscList $ typeAttrs k

attrClosure :: TypeAttr -> S.Set TypeAttr
attrClosure attr =
  case attr of
    Sized ->
      S.singleton Sized
    Actual ->
      S.singleton Actual

reifyAttr :: TypeAttr -> T.Text
reifyAttr attr =
  case attr of
    Sized ->
      "sized"
    Actual ->
      "actual"

reifyUnsafeAttr :: TypeAttr -> T.Text
reifyUnsafeAttr attr =
  "unsafe-" <> reifyAttr attr

deriveBinary [''TypeAttr, ''CopyKind, ''VarKind]
