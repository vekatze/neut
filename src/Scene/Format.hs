module Scene.Format (format) where

import Context.App
import Context.Parse (readSourceFile)
import Control.Monad
import Data.Text qualified as T
import Entity.Ens.Reify qualified as Ens
import Entity.FileType qualified as FT
import Entity.RawProgram.Decode qualified as RawProgram
import Path
import Scene.Ens.Reflect qualified as Ens
import Scene.Parse.Core qualified as P
import Scene.Parse.Program qualified as Parse
import Prelude hiding (log)

format :: FT.FileType -> Path Abs File -> App T.Text
format fileType path = do
  case fileType of
    FT.Ens -> do
      Ens.pp <$> Ens.fromFilePath path
    FT.Source -> do
      content <- readSourceFile path
      program <- P.parseFile True Parse.parseProgram path content
      return $ RawProgram.pp program
