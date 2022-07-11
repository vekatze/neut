module Entity.ModuleID
  ( ModuleID (..),
    getModuleID,
    reify,
  )
where

import Data.Binary
import Data.Hashable
import qualified Data.Text as T
import Entity.Module
import qualified Entity.ModuleChecksum as MC
import GHC.Generics
import Path
import qualified System.FilePath as FP

data ModuleID
  = This
  | That MC.ModuleChecksum
  | Base
  deriving (Generic, Eq, Ord, Show)

instance Binary ModuleID

instance Hashable ModuleID

getModuleID :: Module -> Module -> ModuleID
getModuleID mainModule currentModule = do
  if moduleLocation mainModule == moduleLocation currentModule
    then This
    else
      That $
        MC.ModuleChecksum $
          T.pack $ FP.dropTrailingPathSeparator $ toFilePath $ dirname $ parent (moduleLocation currentModule)

reify :: ModuleID -> T.Text
reify moduleID =
  case moduleID of
    This ->
      "this"
    That checksum ->
      MC.reify checksum
    Base ->
      "base"
