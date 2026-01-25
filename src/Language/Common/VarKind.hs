module Language.Common.VarKind
  ( VarKind (..),
    isExp,
  )
where

import Data.Binary (Binary)
import GHC.Generics (Generic)

data VarKind
  = Exp
  | Normal
  deriving (Eq, Ord, Show, Generic)

instance Binary VarKind

isExp :: VarKind -> Bool
isExp k =
  case k of
    Exp -> True
    Normal -> False
