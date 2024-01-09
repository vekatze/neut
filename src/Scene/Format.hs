module Scene.Format (format) where

import Context.App
import Control.Monad
import Data.Text qualified as T
import Entity.Ens.Reify qualified as Ens
import Entity.FileType qualified as FT
import Entity.RawProgram.Decode qualified as RawProgram
import Path
import Scene.Ens.Reflect qualified as Ens
import Scene.Module.GetEnabledPreset
import Scene.Module.Reflect qualified as Module
import Scene.Parse.Core qualified as P
import Scene.Parse.Program qualified as Parse
import Prelude hiding (log)

format :: FT.FileType -> Path Abs File -> T.Text -> App T.Text
format fileType path content = do
  case fileType of
    FT.Ens -> do
      Ens.pp <$> Ens.fromFilePath' path content
    FT.Source -> do
      baseModule <- Module.fromCurrentPath
      activePresetNames <- getEnabledPreset baseModule
      program <- P.parseFile True Parse.parseProgram path content
      return $ RawProgram.pp undefined program
