module Language.Common.PiKind
  ( PiKind (..),
    normal,
  )
where

import Data.Binary
import GHC.Generics (Generic)
import Language.Common.IsConstLike (IsConstLike)

data PiKind
  = Normal IsConstLike
  | DataIntro IsConstLike
  deriving (Show, Eq, Generic)

instance Binary PiKind

normal :: PiKind
normal =
  Normal False
