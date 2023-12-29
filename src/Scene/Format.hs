module Scene.Format (format) where

import Context.App
import Context.Module qualified as Module
import Context.Parse (readSourceFile)
import Control.Monad
import Data.Text qualified as T
import Entity.Const
import Entity.Ens.Reify qualified as Ens
import Entity.RawProgram.Decode qualified as RawProgram
import Path
import Scene.Ens.Reflect qualified as Ens
import Scene.Initialize qualified as Initialize
import Scene.Parse.Core qualified as P
import Scene.Parse.Program qualified as Parse
import Prelude hiding (log)

format :: Path Abs File -> App T.Text
format path = do
  (_, ext) <- splitExtension path
  if ext == moduleFileExtension
    then Ens.pp <$> Ens.fromFilePath path
    else do
      source <- Module.sourceFromPath path
      Initialize.initializeForSource source
      content <- readSourceFile path
      program <- P.parseFile True Parse.parseProgram path content
      return $ RawProgram.pp program
