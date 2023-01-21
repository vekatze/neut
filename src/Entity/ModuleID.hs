module Entity.ModuleID
  ( ModuleID (..),
    reify,
  )
where

import Data.Binary
import Data.Hashable
import Data.Text qualified as T
import Entity.ModuleChecksum qualified as MC
import GHC.Generics

data ModuleID
  = Main
  | Library MC.ModuleChecksum
  | Base
  deriving (Generic, Eq, Ord, Show)

instance Binary ModuleID

instance Hashable ModuleID

reify :: ModuleID -> T.Text
reify moduleID =
  case moduleID of
    Main ->
      "this"
    Library checksum ->
      MC.reify checksum
    Base ->
      "base"
