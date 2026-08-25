module Language.Common.VarKind
  ( VarKind (..),
    TypeAttr (..),
    normal,
    exponential,
    withSource,
    withAttr,
    hasAttr,
    attrList,
    attrClosure,
    reifyAttr,
  )
where

import Data.Binary (Binary)
import Data.Set qualified as S
import Data.Text qualified as T
import GHC.Generics (Generic)

data TypeAttr
  = Sized
  | Actual
  | Integer
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance Binary TypeAttr

data VarKind = VarKind
  { isExp :: Bool,
    isSource :: Bool,
    typeAttrs :: S.Set TypeAttr
  }
  deriving (Eq, Ord, Show, Generic)

instance Binary VarKind

normal :: VarKind
normal =
  VarKind {isExp = False, isSource = False, typeAttrs = S.empty}

exponential :: VarKind
exponential =
  VarKind {isExp = True, isSource = False, typeAttrs = S.empty}

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
    Integer ->
      S.fromList [Integer, Actual]

reifyAttr :: TypeAttr -> T.Text
reifyAttr attr =
  case attr of
    Sized ->
      "sized"
    Actual ->
      "actual"
    Integer ->
      "integer"
