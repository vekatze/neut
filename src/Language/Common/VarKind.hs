module Language.Common.VarKind
  ( VarKind (..),
    normal,
    exponential,
    withSource,
    withSized,
  )
where

import Data.Binary (Binary)
import GHC.Generics (Generic)

data VarKind = VarKind
  { isExp :: Bool,
    isSource :: Bool,
    isSized :: Bool
  }
  deriving (Eq, Ord, Show, Generic)

instance Binary VarKind

normal :: VarKind
normal =
  VarKind {isExp = False, isSource = False, isSized = False}

exponential :: VarKind
exponential =
  VarKind {isExp = True, isSource = False, isSized = False}

withSource :: VarKind -> VarKind
withSource k =
  k {isSource = True}

withSized :: VarKind -> VarKind
withSized k =
  k {isSized = True}
