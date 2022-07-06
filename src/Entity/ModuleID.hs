module Entity.ModuleID
  ( ModuleID (..),
    getModuleID,
  )
where

import qualified Data.Text as T
import Entity.Module
import Entity.ModuleChecksum
import Path
import qualified System.FilePath as FP

data ModuleID
  = This
  | That ModuleChecksum

getModuleID :: Module -> Module -> ModuleID
getModuleID mainModule currentModule = do
  if moduleLocation mainModule == moduleLocation currentModule
    then This
    else
      That $
        ModuleChecksum $
          T.pack $ FP.dropTrailingPathSeparator $ toFilePath $ dirname $ parent (moduleLocation currentModule)
