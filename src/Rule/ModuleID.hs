module Rule.ModuleID
  ( ModuleID (..),
    reify,
  )
where

import Data.Binary
import Data.Hashable
import Data.Text qualified as T
import Rule.ModuleDigest qualified as MD
import GHC.Generics

data ModuleID
  = Main
  | Library MD.ModuleDigest
  | Base
  deriving (Generic, Eq, Ord, Show)

instance Binary ModuleID

instance Hashable ModuleID

reify :: ModuleID -> T.Text
reify moduleID =
  case moduleID of
    Main ->
      "this"
    Library digest ->
      MD.reify digest
    Base ->
      "base"
