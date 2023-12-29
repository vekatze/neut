module Scene.Format (format) where

import Context.App
import Context.Parse (readSourceFile)
import Control.Monad
import Data.Text qualified as T
import Entity.RawProgram.Decode qualified as RawProgram
import Path
import Scene.Parse.Core qualified as P
import Scene.Parse.Program qualified as Parse
import Prelude hiding (log)

format :: Path Abs File -> App T.Text
format path = do
  content <- readSourceFile path
  program <- P.parseFile True Parse.parseProgram path content
  return $ RawProgram.pp program
